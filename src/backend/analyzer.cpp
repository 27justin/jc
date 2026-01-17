#include <cassert>
#include <iostream>
#include <memory>
#include <sstream>

#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "frontend/ast.hpp"
#include "frontend/diagnostic.hpp"
#include "frontend/token.hpp"

semantic_info_t
analyzer_t::analyze(translation_unit_t tu) {
  semantic_info_t info {
    .unit = tu
  };

  push_scope();
  for (auto &decl : info.unit.declarations) {
    try {
      resolved_types[decl] = analyze_node(decl);
    } catch (const diagnostic_t &msg) {
      diagnostics.messages.push_back(msg);
    }
  }
  pop_scope();

  info.resolved_types = std::move(resolved_types);

  if (diagnostics.messages.size() > 0) {
    throw analyze_error_t {std::move(diagnostics)};
  }
  return info;
}

SP<scope_t>
analyzer_t::push_scope() {
  SP<scope_t> parent { nullptr };
  if (scope_stack.size() > 0)
    parent = scope_stack.back();
  scope_stack.emplace_back(std::make_shared<scope_t>(parent));
  return scope_stack.back();
}

void analyzer_t::pop_scope() {
  scope_stack.pop_back();
}

SP<scope_t>
analyzer_t::get_scope() {
  return scope_stack.back();
}

void
analyzer_t::push_type_infer(SP<type_t> ty) {
  type_infer_stack.push_back(ty);
}

void analyzer_t::pop_type_infer() {
  type_infer_stack.pop_back();
}

SP<type_t>
analyzer_t::get_type_infer() {
  return type_infer_stack.back();
}

bool
analyzer_t::has_type_infer() {
  return type_infer_stack.size() > 0;
}

void
analyzer_t::push_function_frame(SP<type_t> type) {
  function_stack.emplace_back(type);
}

void analyzer_t::pop_function_frame() {
  function_stack.pop_back();
}

SP<type_t>
analyzer_t::get_function_frame() {
  return function_stack.back();
}

bool
analyzer_t::has_function_frame() {
  return !function_stack.empty();
}

SP<type_t>
analyzer_t::analyze_literal(SP<ast_node_t> node) {
  literal_expr_t *expr = node->as.literal_expr;

  SP<type_t> result;
  switch (expr->type) {
  case literal_type_t::eString: {
    // String literals are never mutable.
    result = types.pointer_to(types.resolve("u8"), {pointer_kind_t::eNonNullable}, false);
    break;
  }
  case literal_type_t::eInteger: {
    result = types.resolve("i32");
    break;
  }
  case literal_type_t::eBool: {
    result = types.resolve("bool");
    break;
  }
  case literal_type_t::eFloat: {
    result = types.resolve("f32");
    break;
  }
  }
  return result;
}

SP<type_t>
analyzer_t::analyze_function_parameter(SP<ast_node_t> node) {
  auto param = node->as.fn_param;

  // Otherwise normal binding
  if (param->type) {
    auto type = analyze_node(param->type);
    return type;
  }
  return types.resolve("void");
}

SP<type_t>
analyzer_t::analyze_function_decl(SP<ast_node_t> node) {
  auto decl = node->as.fn_decl;

  SP<type_t> return_type{nullptr},
             receiver {nullptr}; //< Implicitly takes `receiver` as the first argument

  if (decl->type)
    return_type = analyze_node(decl->type);
  else
    // Default to void
    return_type = types.resolve("void");

  // The function name can be namespaced.  The last element of the
  // path is the function name, the remainder the type, or namespace.

  string_list path = split(decl->name, ".");
  auto function_name = path.back();
  path.pop_back();

  // With path, we can now check if it is a type.
  if (receiver = types.resolve(join(path, ".")); receiver) {
    // It is a type, this is important if the function takes a self
    // parameters, that is not specified.
    push_type_infer(receiver);
  }

  std::vector<SP<type_t>> parameters;

  auto it = decl->parameters.begin();

  // With the information about `receiver`, we can now check for the
  // presence of a `self` parameter.
  if (decl->parameters.size() > 0) {
    auto &first = (*it)->as.fn_param;
    if (first->is_self) {
      if (!first->type) {
        // This implies that the function takes `receiver` as the first argument
        if (first->is_self_ref) //< Pass-by-reference
          parameters.push_back(types.pointer_to(receiver, {pointer_kind_t::eNonNullable}, first->is_mutable));
        else // Pass-by-value
          parameters.push_back(receiver);
      } else {
        // The receiver is specified, use that.
        parameters.push_back(analyze_node(first->type));
      }
      // Store the type back on the node
      (*it)->type = parameters.back();
      // Forward the parameters, since this one is 'hidden'
      it++;
    } else {
      // If we don't have a receiver, this function is static. In that
      // case, we don't have to bother saving it.
      receiver = nullptr;
    }
  }

  for (; it != decl->parameters.end(); ++it) {
    auto param = *it;
    auto ptype = analyze_function_parameter(param);
    param->type = ptype;
    parameters.push_back(ptype);
  }

  if (receiver) {
    // Remove type from infer stack
    pop_type_infer();
  }

  SP<type_t> fn_type = types.add_function(return_type, parameters, receiver, decl->is_var_args);
  get_scope()->add(decl->name, fn_type);
  return fn_type;
}


SP<type_t>
analyzer_t::analyze_function(SP<ast_node_t> node) {
  auto impl = node->as.fn_impl;

  auto base_type = analyze_node(impl->declaration);

  // Push a new scope & function frame, then analyze the block.
  push_scope();
  push_function_frame(base_type);

  // Bind our parameters to the scope
  function_decl_t *header = impl->declaration->as.fn_decl;
  function_signature_t *signature = base_type->as.function;

  for (auto i = 0; i < signature->arg_types.size(); ++i) {
    auto param = header->parameters[i];
    auto fn_param = param->as.fn_param;
    get_scope()->add(fn_param->name, signature->arg_types[i], fn_param->is_mutable);
  }

  if (signature->receiver)
    push_type_infer(signature->receiver);

  analyze_node(impl->block);

  if (signature->receiver)
    pop_type_infer();

  pop_function_frame();
  pop_scope();

  return base_type;
}

SP<type_t>
analyzer_t::analyze_block(SP<ast_node_t> node) {
  auto block = node->as.block;
  for (auto &v : block->body) {
    analyze_node(v);
  }
  // TODO: Implicit return, the last statement in a body should be the
  // type of body.
  return types.resolve("void");
}

SP<type_t>
analyzer_t::analyze_struct(SP<ast_node_t> node) {
  auto decl = node->as.struct_decl;

  // Compute all member & their types, and then compute a memory
  // layout
  struct_layout_t layout {};

  for (auto &member : decl->members) {
    assert(member->kind == ast_node_t::eDeclaration);
    declaration_t *decl = member->as.declaration;
    SP<type_t> type = analyze_node(decl->type);
    layout.members.push_back(struct_layout_t::field_t{
        .name = decl->identifier,
        .type = type
      });
  }

  layout.compute_memory_layout();

  // Create new type.
  return types.add_struct(decl->name, layout);;
}

SP<type_t>
analyzer_t::analyze_type(SP<ast_node_t> node) {
  auto decl = node->as.type;

  auto type = types.resolve(decl->name);
  if (!type) {
    unknown_type_error(node);
  }

  auto sq = type;
  if (decl->indirections.size() > 0) {
    sq = types.pointer_to(sq, decl->indirections, decl->is_mutable);
  }
  return sq;
}

SP<type_t>
analyzer_t::analyze_declaration(SP<ast_node_t> node) {
  auto &decl = *node->as.declaration;

  SP<type_t> type;
  if (decl.type)
    type = analyze_node(decl.type);
  // else, we try to infer from the value.

  bool type_infer = false;
  if (type) {
    push_type_infer(type); // To infer type of `.` shorthand
    type_infer = true;
  }

  if (decl.value) {
    SP<type_t> val_type = analyze_node(decl.value);

    // Type not set yet, we infer.
    if (!type) {
      type = val_type;
    } else {
      // Check for mismatch
      if (!is_coercible(val_type, type)) {
        type_error(type, val_type, decl.value);
      }
    }
  }

  if (type_infer)
    pop_type_infer();

  if (!type) {
    // Unable to infer type.
    infer_error(node);
  } else {
    // Got a type, check that it's valid.
    if (type->size == 0 && type->kind != type_kind_t::ePointer) {
      invalid_type_error(type, node);
    }
  }

  get_scope()->add(decl.identifier, type, decl.is_mutable);
  return type;
}

SP<type_t>
analyzer_t::analyze_return(SP<ast_node_t> node) {
  return_stmt_t *stmt = node->as.return_stmt;

  if (!has_function_frame()) {
    throw error(source, node->location, INVALID_RETURN_STMT, INVALID_RETURN_STMT_DETAIL);
    return nullptr;
  }

  if (stmt->value == nullptr)
    return types.resolve("void");

  auto expected_type = get_function_frame()->as.function->return_type;
  auto ret_type = analyze_node(stmt->value);

  if (ret_type != expected_type
      && !is_coercible(ret_type, expected_type)) {
    type_error(expected_type, ret_type, stmt->value);
  }

  return ret_type;
}

bool analyzer_t::is_coercible(SP<type_t> from,
                              SP<type_t> into) {

  if (from == into) return true;

  // `any` pointer can be coerced into any other pointer.
  if (from->name == "any"
      && from->kind == type_kind_t::ePointer
      && into->kind == type_kind_t::ePointer) {
    auto from_ptr = from->as.pointer;
    auto to_ptr = into->as.pointer;

    auto from_level = from_ptr->indirections.back();
    auto to_level = to_ptr->indirections.back();

    // !any -> var !u8 <- Not OK, constness lost
    if (!from_ptr->is_mutable && to_ptr->is_mutable)
      return false;


    // ?any -> !i32 <- Not OK
    if (from_level == pointer_kind_t::eNullable && to_level == pointer_kind_t::eNonNullable)
      return false;

    return true;
  }

  // Any other pointer can also be cast into `any`
  if (into->name == "any"
      && into->kind == type_kind_t::ePointer
      && from->kind == type_kind_t::ePointer) {

    auto from_ptr = from->as.pointer;
    auto to_ptr = into->as.pointer;

    // !u8 -> var !any <- Not OK, constness lost
    if (!from_ptr->is_mutable && to_ptr->is_mutable)
      return false;

    // ?i32 -> !any
    // Not allowed, breaks nullability.
    if (from_ptr->indirections.back() == pointer_kind_t::eNullable
        && to_ptr->indirections.back() == pointer_kind_t::eNonNullable)
      return false;
    return true;
  }

  if (from->kind == type_kind_t::ePointer &&
      into->kind == type_kind_t::ePointer) {

    auto from_ptr = from->as.pointer;
    auto to_ptr = into->as.pointer;

    auto from_level = from_ptr->indirections.back();
    auto to_level = to_ptr->indirections.back();

    // Cast from any concrete type to any other is disallowed
    if (from_ptr->base != to_ptr->base)
      return false;

    // ?person -> !person <- Not OK
    if (from_level == pointer_kind_t::eNullable &&
        to_level == pointer_kind_t::eNonNullable)
      return false;

    // !person -> var !any <- Not OK, casts away constness
    if (from_ptr->is_mutable == false
        && to_ptr->is_mutable == true)
      return false;

    return true;
  }

  // Numbers can be casted based on size
  if (from->is_numeric() && into->is_numeric()) {
    // Only upcasting into better bitwidth is supported.
    return (into->size > from->size) || into->size == from->size;
  }

  // Pointers can be turned into intptr_t and uintptr_t
  if (from->kind == type_kind_t::ePointer &&
      (into->is_numeric() && into->size == sizeof(void *) * 8)) {
    return true;
  }

  // Alias types are trivially coercible
  if (from->kind == type_kind_t::eAlias)
    return is_coercible(from->as.alias->alias, into);

  if (into->kind == type_kind_t::eAlias)
    return is_coercible(from, into->as.alias->alias);

  return false;
}

bool analyzer_t::is_castable(SP<type_t> from,
                             SP<type_t> into) {
  if (is_coercible(from, into)) {
    return true;
  }

  if (from->kind == type_kind_t::eAlias
      || from->kind == type_kind_t::eOpaque) {
    // Check if the resolved qualified types are coercible.
    return is_castable(from->as.alias->alias, into);
  }

  if (into->kind == type_kind_t::eAlias
      || into->kind == type_kind_t::eOpaque) {
    // Check if the resolved qualified types are coercible.
    return is_castable(from, into->as.alias->alias);
  }

  // Integer can be cast into pointers...
  if (into->kind == type_kind_t::ePointer && from->kind == type_kind_t::eUint) {
    return true;
  }

  // `any` pointers can be casted into any other pointer, if explicit.
  if (into->kind == type_kind_t::ePointer &&
      from->kind == type_kind_t::ePointer) {

    auto from_ptr = from->as.pointer;
    auto to_ptr = into->as.pointer;

    auto from_level = from_ptr->indirections.back();
    auto to_level = to_ptr->indirections.back();

    if (into->name == "any" || from->name == "any") {
      return true;
    }
    return false;
  }
  return false;
}

SP<type_t>
analyzer_t::resolve_primitive_binop(binop_type_t T, SP<type_t> L,
                                    SP<type_t> R) {
  if (L->size > R->size) return L;

  using BT = binop_type_t;

  switch (T) {
  case BT::eLT: // <
  case BT::eGT: // >
  case BT::eEqual: // ==
  case BT::eNotEqual: // !=
  case BT::eAnd: // &&
  case BT::eOr: // &&
  case BT::eGTE: // >=
  case BT::eLTE: // <=
    return types.resolve("bool");
  default:
    break;
  }
  return R;
}

SP<type_t>
analyzer_t::analyze_binop(SP<ast_node_t> node) {
  binop_expr_t *expr = node->as.binop;

  SP<type_t> lty = analyze_node(expr->left);
  SP<type_t> rty = analyze_node(expr->right);

  if (!is_coercible(lty, rty)) {
    invalid_operation(node, lty, rty, expr->op);
    return nullptr;
  }

  auto nty = resolve_primitive_binop(expr->op, lty, rty);
  return nty;
}

SP<type_t>
analyzer_t::analyze_symbol(SP<ast_node_t> node) {
  symbol_expr_t *expr = node->as.symbol;

  auto sym = get_scope()->resolve(expr->identifier);
  if (!sym) {
    throw error(source, node->location, UNKNOWN_SYMBOL, fmt(UNKNOWN_SYMBOL_DETAIL, expr->identifier));
  }

  return sym->type;
}

call_frame_t analyzer_t::prepare_call_frame(call_expr_t *expr) {
  // Calls to functions can happen in two certain configurations
  //
  // 1. Dispatched statically from a type `heap.init`
  // 2. Dispatched via a member access `my_heap.alloc`
  //
  // For one, we don't have to do much.
  // For two, we have to declare the implicit receiver (first `self` argument) on the expr.

  call_frame_t frame;

  // Figure out what the function actually is
  auto fnty = analyze_node(expr->callee);
  if (!fnty) {
    throw error(source, expr->callee->location, UNKNOWN_SYMBOL, fmt(UNKNOWN_SYMBOL_DETAIL, source.string(expr->callee->location)));
  }

  // Check that it is actually callable
  if (fnty->kind != type_kind_t::eFunction) {
        throw error(source, expr->callee->location, NOT_A_FUNCTION,
                    fmt(NOT_A_FUNCTION_DETAIL, source.string(expr->callee->location), to_string(fnty)));
  }

  auto fn = fnty->as.function;
  frame.expected_params = fn->arg_types;
  frame.return_type = fn->return_type;
  frame.is_var_args = fn->is_var_args;

  // Special case for call from a member-access:
  // This is syntactic sugar for:
  // my_symbol.function() -> my_type.function(my_symbol)

  if (expr->callee->kind == ast_node_t::eMemberAccess
      && fn->receiver) {
    member_access_expr_t *mem_expr = expr->callee->as.member_access;

    auto receiver = analyze_node(mem_expr->object);
    if (receiver == fn->receiver) {
      // We can only try to coerce if the receiver types are the same.
      auto self_type = fn->arg_types.front();

      if (!is_coercible(receiver, self_type)) {
        if (self_type->kind == type_kind_t::ePointer) {
          // Pointer types are easy to coerce.  But we only support one
          // level of indirection.
          auto self_receiver = make_node<addr_of_expr_t>(ast_node_t::eAddrOf, {.value = mem_expr->object}, mem_expr->object->location);
          frame.effective_args.push_back(self_receiver);
        }
      } else {
        frame.effective_args.push_back(mem_expr->object);
      }
    }

    // Desugar the member access into the fully qualified symbol
    string_list path;
    flatten_member_access(expr->callee, path);
    desugar<symbol_expr_t>(expr->callee, ast_node_t::eSymbol, {.identifier = join({fn->receiver->name, path.back()}, ".")});
  }

  // Push the expected parameters
  for (auto &arg : expr->arguments) {
    frame.effective_args.push_back(arg);
  }

  return frame;
}

SP<type_t> analyzer_t::analyze_call(SP<ast_node_t> node) {
  call_expr_t *expr = node->as.call_expr;

  auto frame = prepare_call_frame(expr);

  // Verify that we have enough args
  if (frame.effective_args.size() != frame.expected_params.size()
      && !frame.is_var_args) {
    throw error(source, node->location,
                fmt(ARG_COUNT_MISMATCH, frame.effective_args.size() > frame.expected_params.size() ? "many" : "few"),
                fmt(ARG_COUNT_MISMATCH_DETAIL, source.string(expr->callee->location),
                    frame.expected_params.size(), frame.effective_args.size()));
  }

  // Verify that all arguments either match, or are coercible to
  // what they need to be.
  for (size_t i = 0; i < frame.effective_args.size(); ++i) {
    // Analyze the our passed argument first, otherwise the type wont
    // be inferred for the codegen later.
    auto current_ty = analyze_node(frame.effective_args[i]);

    // Skip if we exceed expected args (only when the fn is varargs)
    if (i >= frame.expected_params.size())
      continue;

    auto expected_ty = frame.expected_params[i];

    if (!is_coercible(current_ty, expected_ty)) {
      type_error(expected_ty, current_ty, frame.effective_args[i]);
    }
  }

  expr->arguments = frame.effective_args;
  return frame.return_type;
}

std::string analyzer_t::join(const std::vector<std::string> &list,
                                          const std::string &separator) {
  std::stringstream ss;
  for (int64_t i = 0; i < list.size(); ++i) {
    ss << list[i];
    if (i < list.size() - 1)
      ss << separator;
  }
  return ss.str();
}

std::vector<std::string> analyzer_t::split(const std::string &list,
                                           const std::string &separator) {
  std::string str = list;
  string_list result;
  auto p = str.find(separator);
  while (p != std::string::npos) {
    result.push_back(str.substr(0, p));
    str = str.substr(p + 1);
    p = str.find(separator);
  }
  // Push remaining
  result.push_back(str);
  return result;
}

SP<symbol_t>
analyzer_t::resolve_path(const std::vector<std::string> &path) {
  auto sym = get_scope()->resolve(join(path, "."));
  if (sym) return sym;
  return nullptr;
}

void
analyzer_t::flatten_member_access(SP<ast_node_t> node,
                                       std::vector<std::string> &path) {
  // Flatten member access structures into a flat dot separated list
  if (node->kind == ast_node_t::eSymbol) {
    // Only on symbols, these might be either static, or local.
    path.push_back(node->as.symbol->identifier);
  } else if (node->kind == ast_node_t::eMemberAccess) {
    flatten_member_access(node->as.member_access->object, path);
    path.push_back(node->as.member_access->member);
  }
}

SP<type_t>
analyzer_t::analyze_member_access(SP<ast_node_t> node) {
  member_access_expr_t *expr = node->as.member_access;

  auto scope = get_scope();

  // Member access is more difficult and nuanced.
  //
  // Since we mix namespaces & symbols, a nested member access /might/
  // point into a structure, but it might also point at a symbol
  // (e.g. std.print, or std.io.file)
  //
  // So first, we'll try to collect recursively collect member access
  // objects (as long as they are symbols) into a list.

  string_list path;
  flatten_member_access(node, path);

  // Now, the first step is to check if this symbol exists.
  // This works with, e.g. `std.file.open`
  if (auto sym = scope->resolve(join(path, "."))) {
    // It exists, then this is a static symbol.
    // Desugar this access into a normal symbol.
    desugar<symbol_expr_t>(node, ast_node_t::eSymbol, {.identifier = join(path, ".")});
    return analyze_node(node);
  }

  // Second, we can also try inferring the type, via the type infer cache.
  if (has_type_infer()) {
    auto type_name = split(get_type_infer()->name, ".");
    auto infer_path = path;
    infer_path.insert(infer_path.begin(), type_name.begin(), type_name.end());

    if (auto sym = scope->resolve(join(path, "."))) {
      // It exists, then this is a static symbol.
      // Desugar this access into a normal symbol.
      desugar<symbol_expr_t>(node, ast_node_t::eSymbol, {.identifier = join(path, ".")});
      return analyze_node(node);
    }
  }

  // Now we now look up the type of the object
  auto object = analyze_node(expr->object);

  // Pointers get automatically dereferenced by us.
  if (object->kind == type_kind_t::ePointer) {
    object = object->as.pointer->base;
    expr->object = make_node<deref_expr_t>(ast_node_t::eDeref, {.value = expr->object}, expr->object->location);
    return analyze_member_access(node);
  }

  // If it's a struct, we might be accessing a member.
  if (object->kind == type_kind_t::eStruct) {
    auto layout = object->as.struct_layout;
    if (auto member = layout->member(expr->member)) {
      return member->type;
    }
  }

  // As a last option, we lookup the typename + the member as a
  // symbol, maybe it's that.
  if (auto function =
      scope->resolve(join({object->name, expr->member}, "."))) {
    return function->type;
  }

  unknown_member(expr->member, object, node);
  return types.resolve("void");
}

bool analyzer_t::is_lvalue(SP<ast_node_t> node) {
  switch (node->kind) {
  case ast_node_t::eSymbol:
    return true;
  case ast_node_t::eMemberAccess:
    return is_lvalue(node->as.member_access->object);
  case ast_node_t::eSelf:
    return true;
  case ast_node_t::eDeref:
    return is_lvalue(node->as.deref_expr->value);
  default:
    return false;
  }
}

bool analyzer_t::is_mutable(SP<ast_node_t> node) {
  switch (node->kind) {
  case ast_node_t::eSymbol:
    return get_scope()->resolve(node->as.symbol->identifier)->is_mutable;
  case ast_node_t::eMemberAccess:
    return is_mutable(node->as.member_access->object);
  case ast_node_t::eSelf:
    return get_scope()->resolve("self")->is_mutable;
  case ast_node_t::eDeref:
    return is_mutable(node->as.deref_expr->value);
  default:
    return false;
  }
}

SP<type_t>
analyzer_t::analyze_addr_of(SP<ast_node_t> node) {
  // We can only take the address of locals.
  addr_of_expr_t *addr = node->as.addr_of;

  if (is_lvalue(addr->value)) {
    auto qt = analyze_node(addr->value);
    return types.pointer_to(qt, {pointer_kind_t::eNonNullable}, is_mutable(addr->value));
  }
  generic_error(node, "Illegal addr-of", "Taking the address of a temporary is not allowed.", "The address-of operator (`&`) can only be used on lvalues.");
  return nullptr;
}

SP<type_t> analyzer_t::analyze_extern(SP<ast_node_t> node) {
  extern_decl_t *decl = node->as.extern_decl;

  // Save the information about extern somewhere we can later
  // retrieve, to get weak bindings and not complain about unknown
  // definition.
  auto scope = get_scope();
  auto extern_type = analyze_node(decl->import);
  return extern_type;
}

SP<type_t> analyzer_t::analyze_unary(SP<ast_node_t> node) {
  unary_expr_t *expr = node->as.unary;

  // Shorthand syntax, this can be desugared into [MemberAccess [self] [expr]]
  if (expr->op == token_type_t::operatorDot) {
    // Collect the path
    string_list path;
    flatten_member_access(expr->value, path);

    // Within a function, `.identifier` gets turned into `self.identifier`
    if (has_function_frame() && has_type_infer()) {
      auto receiver = get_function_frame()->as.function->receiver;
      auto infer = get_type_infer();
      // ... but only if the most recent type infer information is the
      // same as the function receiver.
      //
      // This allows us to access self via `.` in receiver functions,
      // but it also allows us to infer static symbols on
      // declarations. (`let x: file = .open()` within function
      // context)
      if (receiver == infer) {
        desugar<member_access_expr_t>(
            node, ast_node_t::eMemberAccess,
            {.object =
                 make_node<self_decl_t>(ast_node_t::eSelf, {}, node->location),
             .member = join(path, ".")});
        return analyze_node(node);
      }
    }

    // Join the shorthand with the most recent type within our
    // inferral stack
    if (has_type_infer()) {
      auto infer_path = split(get_type_infer()->name, ".");
      path.insert(path.begin(), infer_path.begin(), infer_path.end());
      desugar<symbol_expr_t>(node, ast_node_t::eSymbol, {.identifier = join(path, ".")});
      return analyze_node(node);
    }
  }

  // Unaries return the same type.
  auto sq = analyze_node(expr->value);
  return sq;
}

SP<type_t>
analyzer_t::analyze_self(SP<ast_node_t> node) {
  if (!has_function_frame()) {
    generic_error(node, "Unknown self", "`self` is only supported within member functions, usage here is invalid.");
  }

  auto frame = get_function_frame();
  auto fn_sig = frame->as.function;
  return fn_sig->arg_types[0];
}

SP<type_t>
analyzer_t::analyze_if(SP<ast_node_t> node) {
  if_stmt_t *stmt = node->as.if_stmt;
  auto bool_type = types.resolve("bool");

  auto condition_type = analyze_node(stmt->condition);
  if (!is_coercible(condition_type, bool_type)) {
    type_error(bool_type, condition_type, stmt->condition);
  }

  analyze_block(stmt->pass);
  if (stmt->reject)
    analyze_block(stmt->reject);

  return types.resolve("void");
}

SP<type_t>
analyzer_t::analyze_type_alias(SP<ast_node_t> node) {
  type_alias_decl_t *decl = node->as.alias_decl;
  auto base = analyze_node(decl->type);
  return types.add_alias(decl->alias, base, decl->is_distinct);
}

SP<type_t>
analyzer_t::analyze_cast(SP<ast_node_t> node) {
  cast_expr_t *expr = node->as.cast;

  auto target = analyze_node(expr->type);
  auto source = analyze_node(expr->value);

  if (is_castable(source, target)) {
    return target;
  }

  invalid_cast(source, target, expr->value);
  return nullptr;
}

SP<type_t>
analyzer_t::analyze_deref(SP<ast_node_t> node) {
  deref_expr_t *expr = node->as.deref_expr;

  // Retrieve the type of the value
  auto type = analyze_node(expr->value);

  // Deref only works on pointers.
  if (type->kind != type_kind_t::ePointer) {
    invalid_deref(type, node);
  }

  // If valid deref, pop the last indirection
  auto indirections = type->as.pointer->indirections;
  indirections.pop_back();

  if (indirections.size() > 0)
    // We might only strip one pointer level
    return types.pointer_to(type->as.pointer->base, indirections, type->as.pointer->is_mutable);
  else
    // We might also deref to the plain type.
    return type->as.pointer->base;
}

SP<type_t>
analyzer_t::analyze_assignment(SP<ast_node_t> node) {
  assign_expr_t *expr = node->as.assign_expr;

  auto R = analyze_node(expr->value);
  auto L = analyze_node(expr->where);

  if (!is_mutable(expr->where)) {
    var_error(expr->where);
  }

  if (is_coercible(R, L)) {
    // This expression returns the left-hand side.
    return L;
  }
  invalid_assignment(R, expr->where);
  return nullptr;
}

SP<type_t>
analyzer_t::analyze_nil(SP<ast_node_t> node) {
  return types.pointer_to(types.resolve("any"), {pointer_kind_t::eNullable}, false);
}

SP<type_t>
analyzer_t::analyze_attribute(SP<ast_node_t> node) {
  attribute_decl_t *decl = node->as.attribute_decl;
  return analyze_node(decl->affect);
}

SP<type_t>
analyzer_t::analyze_node(SP<ast_node_t> node) {
  SP<type_t> type;

  switch (node->kind) {
  case ast_node_t::eFunctionImpl:
    type = analyze_function(node);
    break;

  case ast_node_t::eFunctionDecl: // Function without body (extern, etc.)
    type = analyze_function_decl(node);
    break;

  case ast_node_t::eDeclaration:
    type = analyze_declaration(node);
    break;

  case ast_node_t::eExtern:
    type = analyze_extern(node);
    break;

  case ast_node_t::eBlock:
    type = analyze_block(node);
    break;

  case ast_node_t::eLiteral:
    type = analyze_literal(node);
    break;

  case ast_node_t::eBinop:
    type = analyze_binop(node);
    break;

  case ast_node_t::eCall:
    type = analyze_call(node);
    break;

  case ast_node_t::eReturn:
    type = analyze_return(node);
    break;

  case ast_node_t::eSymbol:
    type = analyze_symbol(node);
    break;

  case ast_node_t::eType:
    type = analyze_type(node);
    break;

  case ast_node_t::eStructDecl:
    type = analyze_struct(node);
    break;

  case ast_node_t::eMemberAccess:
    type = analyze_member_access(node);
    break;

  case ast_node_t::eAddrOf:
    type = analyze_addr_of(node);
    break;

  case ast_node_t::eUnary:
    type = analyze_unary(node);
    break;

  case ast_node_t::eSelf:
    type = analyze_self(node);
    break;

  case ast_node_t::eIf:
    type = analyze_if(node);
    break;

  case ast_node_t::eTypeAlias:
    type = analyze_type_alias(node);
    break;

  case ast_node_t::eCast:
    type = analyze_cast(node);
    break;

  case ast_node_t::eDeref:
    type = analyze_deref(node);
    break;

  case ast_node_t::eAssignment:
    type = analyze_assignment(node);
    break;

  case ast_node_t::eNil:
    type = analyze_nil(node);
    break;

  case ast_node_t::eAttribute:
    type = analyze_attribute(node);
    break;

  default:
    assert(false && "Internal compiler error: Unhandled AST node");
  }

  node->type = type;
  return type;
}

void analyzer_t::type_error(SP<type_t> expected, SP<type_t> got, SP<ast_node_t> where) {
  throw error(source, where->location,
              ILLEGAL_TYPE, fmt(ILLEGAL_TYPE_DETAIL, to_string(got), to_string(expected)));
}

void analyzer_t::var_error(SP<ast_node_t> where) {
  throw error(source, where->location,
              MUTABILITY_VIOLATION, fmt(MUTABILITY_VIOLATION_DETAIL, source.string(where->location)), MUTABILITY_VIOLATION_RECOMMEND);
}

void analyzer_t::unknown_type_error(SP<ast_node_t> where) {
  throw error(source, where->location,
              UNKNOWN_TYPE, fmt(UNKNOWN_TYPE_DETAIL, source.string(where->location)));
}

void analyzer_t::infer_error(SP<ast_node_t> where) {
  throw error(source, where->location,
              UNKNOWN_TYPE,
              INFER_DETAIL,
              fmt(INFER_RECOMMEND, source.string(where->location)));
}

void analyzer_t::invalid_type_error(SP<type_t> ty, SP<ast_node_t> where) {
  throw error(source, where->location,
              INVALID_TYPE_ASSIGNMENT,
              fmt(INVALID_TYPE_ASSIGNMENT_DETAIL, to_string(ty)));
}

void analyzer_t::invalid_assignment(SP<type_t> ty, SP<ast_node_t> where) {
  throw error(source, where->location,
              INVALID_ASSIGNMENT,
              fmt(INVALID_ASSIGNMENT_DETAIL, to_string(ty), to_string(where->type)));
}

void analyzer_t::unknown_symbol(SP<ast_node_t> node) {
  throw error(source, node->location,
              UNKNOWN_SYMBOL,
              fmt(UNKNOWN_SYMBOL_DETAIL, source.string(node->location)));
}

void analyzer_t::unknown_member(const std::string &member, SP<type_t> type, SP<ast_node_t> where) {
  throw error(source, where->location,
              UNKNOWN_MEMBER,
              fmt(UNKNOWN_MEMBER_DETAIL, member, to_string(type)));
}

void analyzer_t::invalid_deref(SP<type_t> type, SP<ast_node_t> where) {
  throw error(source, where->location,
              INVALID_DEREF,
              fmt(INVALID_DEREF_DETAIL, to_string(type)));
}

void analyzer_t::invalid_cast(SP<type_t> from, SP<type_t> into, SP<ast_node_t> where) {
  throw error(source, where->location,
              INVALID_CAST,
              fmt(INVALID_CAST_DETAIL, to_string(from), to_string(into)));
}

void analyzer_t::generic_error(SP<ast_node_t> where, std::string msg, std::string detail, std::string recommendation) {
  throw error(source, where->location, msg, detail,
    recommendation);
}

void analyzer_t::invalid_operation(SP<ast_node_t> node, SP<type_t> L, SP<type_t> R, binop_type_t op) {
  std::string operation;
  using BT = binop_type_t;
  switch (op) {
  case BT::eAdd:
    operation = "+";
    break;
  case BT::eAnd:
    operation = "&&";
    break;
  case BT::eDivide:
    operation = "/";
    break;
  case BT::eEqual:
    operation = "==";
    break;
  case BT::eGT:
    operation = ">";
    break;
  case BT::eGTE:
    operation = ">=";
    break;
  case BT::eLT:
    operation = "<";
    break;
  case BT::eLTE:
    operation = "<=";
    break;
  case BT::eMultiply:
    operation = "*";
    break;
  case BT::eNotEqual:
    operation = "!=";
    break;
  case BT::eOr:
    operation = "||";
    break;
  case BT::eSubtract:
    operation = "-";
    break;
  }

  throw error(source, node->location,
              ILLEGAL_OPERATION, fmt(ILLEGAL_OPERATION_DETAIL, operation, to_string(L), to_string(R)));
}
