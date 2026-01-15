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
analyzer_t::push_type(SP<qualified_type_t> ty) {
  type_infer_stack.push_back(ty);
}

void analyzer_t::pop_type() {
  type_infer_stack.pop_back();
}

SP<qualified_type_t>
analyzer_t::get_type() {
  return type_infer_stack.back();
}

void
analyzer_t::push_function_frame(SP<qualified_type_t> type) {
  function_stack.emplace_back(type);
}

void analyzer_t::pop_function_frame() {
  function_stack.pop_back();
}

SP<qualified_type_t>
analyzer_t::get_function_frame() {
  return function_stack.back();
}

bool
analyzer_t::has_function_frame() {
  return !function_stack.empty();
}

SP<qualified_type_t>
analyzer_t::analyze_literal(SP<ast_node_t> node) {
  literal_expr_t *expr = node->as.literal_expr;

  SP<qualified_type_t> result;
  switch (expr->type) {
  case literal_type_t::eString: {
    // TODO: We have no real syntax for type constness yet, therefore
    // we mark everything as mutable for now.
    result = types.get_qualified(types.resolve("u8"), true, false, false/*true*/);
    break;
  }
  case literal_type_t::eInteger: {
    result = types.get_qualified(types.resolve("i32"), false, false, false);
    break;
  }
  case literal_type_t::eBool: {
    result = types.get_qualified(types.resolve("bool"), false, false, false);
    break;
  }
  case literal_type_t::eFloat: {
    result = types.get_qualified(types.resolve("f32"), false, false, false);
    break;
  }
  }
  resolved_types[node] = result;
  return result;
}

SP<qualified_type_t>
analyzer_t::analyze_function_parameter(SP<ast_node_t> node) {
  auto param = node->as.fn_param;

  if (param->is_self) {
    // Infer from type infer stack
    if (param->type == nullptr) {
      auto base_type = get_type();
      return types.get_qualified(base_type->base, param->is_self_ref, false, param->is_const);
    } else {
      // Self is a specific instance
      auto qt = analyze_type(param->type);
      return types.get_qualified(qt->base, qt->is_pointer, qt->is_nullable, param->is_const);
    }
  }

  // Otherwise normal binding
  auto qt = analyze_type(param->type);
  return types.get_qualified(qt->base, qt->is_pointer, qt->is_nullable, qt->is_const);
}

SP<qualified_type_t>
analyzer_t::analyze_function_decl(SP<ast_node_t> node) {
  auto decl = node->as.fn_decl;

  SP<qualified_type_t> return_type{nullptr},
    receiver {nullptr}; //< Member function of `receiver`

  if (decl->type)
    return_type = analyze_type(decl->type);
  else
    // Default to void
    return_type = types.get_qualified(types.resolve("void"), false, false, false);

  // Function names can be namespaced, this means we need to
  // this makes them member functions.
  //
  // Futhermore, to allow for implicit pointer receiver functions
  // (non-static member functions) we allow a `self` keyword, to
  // correctly resolve the parameters, we have to push the functions
  // base type (if any) to the type stack, so
  // analyze_function_parameter can lookup the receiver of the
  // function.

  string_list path = split(decl->name, ".");
  bool member_function = false, //< Function exists on some defined
                                //type, and acts though a member of
                                //it.
    static_function = true; //< Function has no `self` receiver
  if (path.size() > 1) {
    path.pop_back(); // Remove the symbol (fn name)
    // Now look up the type
    if (auto type = types.resolve(join(path, ".")); type) {
      // Type exists, good.
      push_type(types.get_qualified(type, false, false, false));
      member_function = true;
    }
  }

  std::vector<SP<qualified_type_t>> parameters;
  for (int64_t i = 0; i < decl->parameters.size(); i++) {
    auto param = decl->parameters[i];
    if (param->as.fn_param->is_self) {
      static_function = false;
    }

    auto ptype = analyze_function_parameter(param);
    resolved_types[param] = ptype;
    parameters.push_back(ptype);
  }

  if (member_function == true && static_function == false) {
    receiver = parameters[0];
  }

  if (member_function == true) {
    // Remove type from infer stack
    pop_type();
  }

  SP<type_t> fn_base_type = types.add_function(return_type, parameters, receiver, decl->is_var_args);
  SP<qualified_type_t> fn_type = types.get_qualified(fn_base_type, true, false, true);
  fn_type->base = fn_base_type;

  fn_type->is_pointer = false;
  fn_type->is_nullable = false;
  fn_type->is_const = true;

  get_scope()->add(decl->name, fn_type);
  resolved_types[node] = fn_type;
  return fn_type;
}


SP<qualified_type_t>
analyzer_t::analyze_function(SP<ast_node_t> node) {
  auto impl = node->as.fn_impl;

  auto base_type = analyze_function_decl(impl->declaration);

  // Push a new scope & function frame, then analyze the block.
  push_scope();
  push_function_frame(base_type);

  // Bind our parameters to the scope
  function_decl_t *header = impl->declaration->as.fn_decl;
  function_signature_t *signature = base_type->base->as.function;

  for (auto i = 0; i < signature->arg_types.size(); ++i) {
    auto param = header->parameters[i];
    if (param->type == ast_node_t::eSelf) {
      // Special case for `self`
      get_scope()->add("self", signature->arg_types[i]);
      continue;
    }
    auto decl = param->as.declaration;
    get_scope()->add(decl->identifier, signature->arg_types[i]);
  }

  if (signature->receiver)
    push_type(signature->receiver);

  analyze_node(impl->block);

  if (signature->receiver)
    pop_type();

  pop_function_frame();
  pop_scope();

  return base_type;
}

SP<qualified_type_t>
analyzer_t::analyze_block(SP<ast_node_t> node) {
  auto block = node->as.block;
  for (auto &v : block->body) {
    analyze_node(v);
  }
  // TODO: Implicit return, the last statement in a body should be the
  // type of body.
  return types.get_qualified(types.resolve("void"), false, false, false);
}

SP<qualified_type_t>
analyzer_t::analyze_struct(SP<ast_node_t> node) {
  auto decl = node->as.struct_decl;

  // Compute all member & their types, and then compute a memory
  // layout
  struct_layout_t layout {};

  for (auto &member : decl->members) {
    assert(member->type == ast_node_t::eDeclaration);
    declaration_t *decl = member->as.declaration;
    SP<qualified_type_t> type = analyze_type(decl->type);
    layout.members.push_back(struct_layout_t::field_t{
        .name = decl->identifier,
        .type = type
      });
  }

  layout.compute_memory_layout();

  // Create new type.
  types.add_struct(decl->name, layout);
  return nullptr;
}

SP<qualified_type_t>
analyzer_t::analyze_type(SP<ast_node_t> node) {
  auto &tydecl = *node->as.type;

  auto type = types.resolve(tydecl.name);
  if (!type) {
    unknown_type_error(node);
  }

  auto sq = types.get_qualified(type, tydecl.is_pointer, tydecl.is_nullable, false);
  resolved_types[node] = sq;
  return sq;
}

SP<qualified_type_t>
analyzer_t::analyze_declaration(SP<ast_node_t> node) {
  auto &decl = *node->as.declaration;

  SP<qualified_type_t> type;
  if (decl.type)
    type = analyze_type(decl.type);
  // else, we try to infer from the value.

  bool type_infer = false;
  if (type) {
    push_type(type); // To infer type of `.` shorthand
    type_infer = true;
  }

  if (decl.value) {
    SP<qualified_type_t> val_type = analyze_node(decl.value);
    resolved_types[decl.value] = val_type;

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
    pop_type();

  // Const by default
  if (type) {
    if (decl.is_mutable) {
      // Only when our variable is `var`, will we allow changes to the
      // pointer.
      type->is_const = false;
    } else {
      type->is_const = true;
    }
  }

  if (!type) {
    // Unable to infer type.
    infer_error(node);
  } else {
    // Got a type, check that it's valid.
    if (type->base->size == 0 && !type->is_pointer) {
      invalid_type_error(type, node);
    }
  }

  // Store the type back into our declaration
  if (!decl.type) {
    decl.type = std::make_shared<ast_node_t>();
    decl.type->type = ast_node_t::eType;
    decl.type->as.type = new ast_type_t{};
    decl.type->as.type->is_nullable = type->is_nullable;
    decl.type->as.type->is_pointer = type->is_pointer;
    decl.type->as.type->name = type->base->name;
    decl.type->location = decl.value->location;
    resolved_types[decl.type] = type;
  }

  resolved_types[node] = type;
  get_scope()->add(decl.identifier, type);
  return type;
}

SP<qualified_type_t>
analyzer_t::analyze_return(SP<ast_node_t> node) {
  return_stmt_t *stmt = node->as.return_stmt;

  if (!has_function_frame()) {
    throw error(source, node->location, INVALID_RETURN_STMT, INVALID_RETURN_STMT_DETAIL);
    return nullptr;
  }

  auto expected_type = get_function_frame()->base->as.function->return_type;
  auto ret_type = analyze_node(stmt->value);

  if (ret_type != expected_type
      && !is_coercible(ret_type, expected_type)) {
    type_error(expected_type, ret_type, stmt->value);
  }

  return ret_type;
}

bool analyzer_t::is_coercible(SP<qualified_type_t> from,
                              SP<qualified_type_t> into) {

  if (from == into) return true;

  // `any` pointer can be coerced into any other pointer.
  if (from->base == types.resolve("any")
      && from->is_pointer
      && into->is_pointer) {

    // ... But only we cast from either null to null, or strong to null.
    if ((from->is_nullable && into->is_nullable)
        || (!from->is_nullable && into->is_nullable))
    return true;
  }

  // Any other pointer can also be cast into `any`
  if (into->base == types.resolve("any")
      && into->is_pointer
      && from->is_pointer) {

    // ?i32 -> !any
    // Not allowed, breaks nullability.
    if (from->is_nullable && !into->is_nullable)
      return false;

    return true;
  }

  // Numbers can be casted based on size & signedness
  if (from->base->is_numeric() && into->base->is_numeric()
      && (!from->is_pointer && !into->is_pointer)) {
    if (from->base->kind == into->base->kind) {
      return (into->base->size > from->base->size) || into->base->size == from->base->size;
    }
  }

  // Any pointer that is non-const, can be cast into a pointer of the
  // same type that is const.
  if (from->is_pointer && into->is_pointer && from->base == into->base &&
      !from->is_const && into->is_const)
    return true;

  // Any pointer that is non-nullable can be cast into a pointer of
  // the same type that is nullable.
  if (from->is_pointer && into->is_pointer && from->base == into->base &&
      !from->is_nullable && into->is_nullable)
    return true;

  // Any concrete type can be cast into itself, if constness differ (copy-by-value).
  if (!from->is_pointer && !into->is_pointer && from->base == into->base)
    return true;

  // Alias types are coercible
  if (from->base->kind == type_t::kind_t::eAlias)
    return is_coercible(from->base->as.alias->alias, into);

  if (into->base->kind == type_t::kind_t::eAlias)
    return is_coercible(from, into->base->as.alias->alias);

  return false;
}

bool analyzer_t::is_castable(SP<qualified_type_t> from,
                             SP<qualified_type_t> into) {
  if (is_coercible(from, into)) {
    return true;
  }

  // Handle aliases
  auto from_base = from->base;
  auto into_base = into->base;

  if (from_base->kind == type_t::kind_t::eAlias
      || from_base->kind == type_t::kind_t::eOpaque) {
    // Check if the resolved qualified types are coercible.
    return is_castable(from_base->as.alias->alias, into);
  }

  if (into_base->kind == type_t::kind_t::eAlias
      || into_base->kind == type_t::kind_t::eOpaque) {
    // Check if the resolved qualified types are coercible.
    return is_castable(from, into_base->as.alias->alias);
  }

  return false;
}

SP<qualified_type_t>
analyzer_t::resolve_primitive_binop(token_type_t T, SP<qualified_type_t> L,
                                    SP<qualified_type_t> R) {
  if (L->base->size > R->base->size) return L;

  switch (T) {
  case token_type_t::delimiterLAngle: // <
  case token_type_t::delimiterRAngle: // >
  case token_type_t::operatorEquality: // ==
  case token_type_t::operatorNotEqual: // !=
    return types.get_qualified(types.resolve("bool"), false, false, false);
  default:
    break;
  }
  return R;
}

SP<qualified_type_t>
analyzer_t::analyze_binop(SP<ast_node_t> node) {
  binop_expr_t *expr = node->as.binop;

  SP<qualified_type_t> lty = analyze_node(expr->left);
  resolved_types[expr->left] = lty;

  SP<qualified_type_t> rty = analyze_node(expr->right);
  resolved_types[expr->right] = rty;

  if (!is_coercible(lty, rty)) {
    throw error(source, node->location,
                                         ILLEGAL_OPERATION, fmt(ILLEGAL_OPERATION_DETAIL, to_text(expr->op), (std::string) *lty, (std::string) *rty));
    return nullptr;
  }

  // Check for certain operators (=), for those we have to validate
  // mutability.
  switch (expr->op) {
  case token_type_t::operatorEqual: // Assignment
    if (lty->is_const) {
      var_error(node);
    }
    break;
  default:
    break;
  }


  auto nty = resolve_primitive_binop(expr->op, lty, rty);
  resolved_types[node] = nty;
  return nty;
}

SP<qualified_type_t>
analyzer_t::analyze_symbol(SP<ast_node_t> node) {
  symbol_expr_t *expr = node->as.symbol;

  auto sym = get_scope()->resolve(expr->identifier);
  if (!sym) {
    throw error(source, node->location, UNKNOWN_SYMBOL, fmt(UNKNOWN_SYMBOL_DETAIL, expr->identifier));
    return nullptr;
  }

  resolved_types[node] = sym->type;
  return sym->type;
}

SP<ast_node_t> wrap_unary(SP<ast_node_t> value, token_type_t tt) {
  auto wrapper = std::make_shared<ast_node_t>();

  // Set the node kind to Unary
  wrapper->type = ast_node_t::eUnary;
  wrapper->location = value->location; // Map back to the original symbol

  wrapper->as.unary = new unary_expr_t {
    .op = tt,
    .value = value
  };
  return wrapper;
}

SP<ast_node_t> wrap_addr_of(SP<ast_node_t> expr) {
  auto addr_of = std::make_shared<ast_node_t>();
  addr_of->location = expr->location;
  addr_of->type = ast_node_t::eAddrOf;
  addr_of->as.addr_of = new addr_of_expr_t(expr);
  return addr_of;
}

SP<ast_node_t> wrap_symbol(const std::string &identifier, source_location_t location) {
  auto sym = std::make_shared<ast_node_t>();
  sym->location = location;
  sym->type = ast_node_t::eSymbol;
  sym->as.symbol = new symbol_expr_t(identifier);
  return sym;
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
  if (fnty->base->kind != type_t::kind_t::eFunction) {
        throw error(source, expr->callee->location, NOT_A_FUNCTION,
                    fmt(NOT_A_FUNCTION_DETAIL, source.string(expr->callee->location), (std::string) *fnty));
  }

  // Check if we are trying to call a member function
  if (expr->callee->type == ast_node_t::eMemberAccess) {
    // Member function, prepare implicit receiver
    frame.effective_args.push_back(expr->callee->as.member_access->object);
    expr->implicit_receiver = expr->callee->as.member_access->object;

    // Flatten the member function from `local.fn` into `type.fn(local)`
    string_list path;
    flatten_member_access(expr->callee, path);
    while (path.size() > 1)
      path.erase(path.begin());

    // Lookup the actual type
    auto qt = analyze_node(expr->callee->as.member_access->object);

    auto sym_name = join({ qt->base->name, path.front() }, ".");
    if (auto sym = get_scope()->resolve(sym_name); sym) {
      expr->callee = wrap_symbol(sym_name, expr->callee->as.member_access->object->location);
    }
  }

  // Push the expected parameters
  auto fn = fnty->base->as.function;
  frame.expected_params = fn->arg_types;
  frame.return_type = fn->return_type;
  frame.is_var_args = fn->is_var_args;

  for (auto &arg : expr->arguments) {
    frame.effective_args.push_back(arg);
  }

  // If we are a calling a member function, we try to auto deref/ref
  // the implicit receiver into what the function wants.
  if (expr->implicit_receiver) {
    auto receiver_type = analyze_node(expr->implicit_receiver);
    auto expected_type = frame.expected_params[0];

    if (is_coercible(receiver_type, expected_type)) {
      // Already good, we can safely implicit cast into expected type.
    } else {
      // Now we need to look at the receiver
      // When our expected_type is a pointer, we can try to take a reference.

      if (expected_type->is_pointer) {
        // Pass by reference

        // Always allowed
        if (expected_type->is_const) {
          expr->implicit_receiver = wrap_addr_of(expr->implicit_receiver);
          frame.effective_args[0] = wrap_addr_of(frame.effective_args[0]);
        }

        // non const only allowed, if receiver_type is also non-const
        if (!expected_type->is_const && receiver_type->is_const) {
          generic_error(expr->implicit_receiver, MUTABILITY_VIOLATION, fmt("This function receives a mutable reference, but `{}` is constant.", source.string(expr->implicit_receiver->location)), fmt("Change the protection of `{}` to var", source.string(expr->implicit_receiver->location)));
        }
      } else {
        // Pass by value
      }
    }
  }

  return frame;
}

SP<qualified_type_t> analyzer_t::analyze_call(SP<ast_node_t> node) {
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

    resolved_types[frame.effective_args[i]] = current_ty;
  }

  resolved_types[node] = frame.return_type;
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
  if (node->type == ast_node_t::eSymbol) {
    // Only on symbols, these might be either static, or local.
    path.push_back(node->as.symbol->identifier);
  } else if (node->type == ast_node_t::eMemberAccess) {
    flatten_member_access(node->as.member_access->object, path);
    path.push_back(node->as.member_access->member);
  }
}

SP<qualified_type_t>
analyzer_t::analyze_member_access(SP<ast_node_t> node) {
  member_access_expr_t *expr = node->as.member_access;

  auto scope = get_scope();

  // Find the "root" of the member access
  SP<ast_node_t> expr_base = expr->object;
  while (expr_base->type == ast_node_t::eMemberAccess) {
    expr_base = expr_base->as.member_access->object;
  }

  // Check that the expression stems from a local type, if it doesn't,
  // we directly have to lookup in global space.
  bool is_local_access = false;
  if (expr_base->type == ast_node_t::eSymbol) {
    // Check if the symbol is a local variable
    if (auto sym = scope->resolve(expr_base->as.symbol->identifier); sym) {
      is_local_access = true;
    }
  }

  if (is_local_access == false) {
    // Not local access, expr->object is either a namespace, or a type.
    string_list path;
    flatten_member_access(node, path);

    if (auto sym = scope->resolve(join(path, ".")); sym) {
      // Rewrite the AST to yield a flat `symbol` for this.
      node->type = ast_node_t::eSymbol;
      node->as.symbol = new symbol_expr_t(join(path, "."));
      return sym->type;
    }
  }

  // First lookup the left side, that has to be known (type, or symbol)
  auto object_type = analyze_node(expr->object);

  // Validate that it's a struct.
  if (object_type->base->kind != type_t::kind_t::eStruct) {

    // Special case: we can add "member" function to any type, even if
    // they are not structs.
    auto sym_name = join({object_type->base->name, expr->member}, ".");
    if (auto sym = get_scope()->resolve(sym_name); sym) {
      return sym->type;
    }

    generic_error(node, "Invalid member access", fmt("Type `{}` is not a struct,member access is illegal.", object_type->base->name));
  }

  // It's a struct.
  // The identifier we are trying to access is either:
  // A. A member of the struct
  // B. A function of the struct
  //
  // First we check the members
  struct_layout_t *layout = object_type->base->as.struct_layout;
  for (auto it = layout->members.begin(); it != layout->members.end(); ++it) {
    if (it->name == expr->member) return it->type;
  }

  // Still nothing, then maybe it's a function.
  std::string symbol_name = fmt("{}.{}", object_type->base->name, expr->member);
  auto fnsym = get_scope()->resolve(symbol_name);
  if (!fnsym) {
    generic_error(node, "Invalid member access", fmt("Member `{}` not found on type `{}`.", expr->member, object_type->base->name));
  }

  resolved_types[node] = fnsym->type;
  return fnsym->type;
}

bool analyzer_t::is_lvalue(SP<ast_node_t> node) {
  switch (node->type) {
  case ast_node_t::eSymbol:
    return true;
  case ast_node_t::eMemberAccess:
    return is_lvalue(node->as.member_access->object);
  case ast_node_t::eSelf:
    return true;
  default:
    return false;
  }
}

SP<qualified_type_t>
analyzer_t::analyze_addr_of(SP<ast_node_t> node) {
  // We can only take the address of locals.
  addr_of_expr_t *addr = node->as.addr_of;

  if (is_lvalue(addr->value)) {
    auto qt = analyze_node(addr->value);
    resolved_types[node] = qt;
    return types.get_qualified(qt->base, true, false, qt->is_const);
  }
  generic_error(node, "Illegal addr-of", "Taking the address of a temporary is not allowed.", "The address-of operator (`&`) can only be used on lvalues.");
  return nullptr;
}

SP<qualified_type_t> analyzer_t::analyze_extern(SP<ast_node_t> node) {
  extern_decl_t *decl = node->as.extern_decl;

  // Save the information about extern somewhere we can later
  // retrieve, to get weak bindings and not complain about unknown
  // definition.
  auto scope = get_scope();
  auto extern_type = analyze_node(decl->import);
  resolved_types[decl->import] = extern_type;
  resolved_types[node] = extern_type;
  return nullptr;
}

SP<qualified_type_t> analyzer_t::analyze_unary(SP<ast_node_t> node) {
  unary_expr_t *expr = node->as.unary;
  // Handle special unaries first (. shorthand)
  if (expr->op == token_type_t::operatorDot
   && expr->value->type == ast_node_t::eSymbol) {
    // Get from type infer cache
    auto infer = get_type();

    // First check if `infer` is a struct, if it is, we might refer to a member.
    if (infer->base->kind == type_t::kind_t::eStruct) {
      struct_layout_t *layout = infer->base->as.struct_layout;
      auto member = layout->member(expr->value->as.symbol->identifier);
      if (member != nullptr) {
        return types.get_qualified(member->type->base, false, false, infer->is_const);
      }
    }

    // If it's not, we likely refer to a symbol on the type.
    string_list path {infer->base->name};
    flatten_member_access(expr->value, path);

    if (auto sym = get_scope()->resolve(join(path, ".")); sym) {
      return sym->type;
    }
  }

  // Else, the unary just returns the same type.
  auto sq = analyze_node(expr->value);
  return sq;
}

SP<qualified_type_t>
analyzer_t::analyze_self(SP<ast_node_t> node) {
  if (!has_function_frame()) {
    generic_error(node, "Unknown self", "`self` is only supported within member functions, usage here is invalid.");
  }

  auto frame = get_function_frame();
  auto fn_sig = frame->base->as.function;

  return fn_sig->arg_types[0];
}

SP<qualified_type_t>
analyzer_t::analyze_if(SP<ast_node_t> node) {
  if_stmt_t *stmt = node->as.if_stmt;
  auto bool_type = types.get_qualified(types.resolve("bool"), false, false, true);

  auto condition_type = analyze_node(stmt->condition);
  if (!is_coercible(condition_type, bool_type)) {
    type_error(bool_type, condition_type, stmt->condition);
  }

  resolved_types[stmt->condition] = condition_type;

  analyze_block(stmt->pass);
  if (stmt->reject)
    analyze_block(stmt->reject);

  return types.get_qualified(types.resolve("void"), false, false, false);
}

SP<qualified_type_t>
analyzer_t::analyze_type_alias(SP<ast_node_t> node) {
  type_alias_decl_t *decl = node->as.alias_decl;
  auto base = analyze_type(decl->type);
  types.add_alias(decl->alias, base, decl->is_distinct);
  return nullptr;
}

SP<qualified_type_t>
analyzer_t::analyze_cast(SP<ast_node_t> node) {
  cast_expr_t *expr = node->as.cast;
  auto target = analyze_type(expr->type);

  if (is_castable(analyze_node(expr->value), target)) {
    return target;
  }
  invalid_type_error(target, expr->value);
}

SP<qualified_type_t>
analyzer_t::analyze_node(SP<ast_node_t> node) {
  switch (node->type) {
  case ast_node_t::eFunctionImpl:
    return analyze_function(node);

  case ast_node_t::eFunctionDecl: // Function without body (extern, etc.)
    return analyze_function_decl(node);

  case ast_node_t::eDeclaration:
    return analyze_declaration(node);

  case ast_node_t::eExtern:
    return analyze_extern(node);

  case ast_node_t::eBlock:
    return analyze_block(node);

  case ast_node_t::eLiteral:
    return analyze_literal(node);

  case ast_node_t::eBinop:
    return analyze_binop(node);

  case ast_node_t::eCall:
    return analyze_call(node);

  case ast_node_t::eReturn:
    return analyze_return(node);

  case ast_node_t::eSymbol:
    return analyze_symbol(node);

  case ast_node_t::eType:
    return analyze_type(node);

  case ast_node_t::eStructDecl:
    return analyze_struct(node);

  case ast_node_t::eMemberAccess:
    return analyze_member_access(node);

  case ast_node_t::eAddrOf:
    return analyze_addr_of(node);

  case ast_node_t::eUnary:
    return analyze_unary(node);

  case ast_node_t::eSelf:
    return analyze_self(node);

  case ast_node_t::eIf:
    return analyze_if(node);

  case ast_node_t::eTypeAlias:
    return analyze_type_alias(node);

  case ast_node_t::eCast:
    return analyze_cast(node);

  default:
    assert(false && "Internal compiler error: Unhandled AST node");
  }

  return nullptr;
}

void analyzer_t::type_error(SP<qualified_type_t> expected, SP<qualified_type_t> got, SP<ast_node_t> where) {
  throw error(source, where->location,
              ILLEGAL_TYPE, fmt(ILLEGAL_TYPE_DETAIL, (std::string) *got, (std::string) *expected));
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

void analyzer_t::invalid_type_error(SP<qualified_type_t> ty, SP<ast_node_t> where) {
  throw error(source, where->location,
              INVALID_TYPE_ASSIGNMENT,
              fmt(INVALID_TYPE_ASSIGNMENT_DETAIL, (std::string) *ty));
}

void analyzer_t::generic_error(SP<ast_node_t> where, std::string msg, std::string detail, std::string recommendation) {
  throw error(source, where->location, msg, detail,
    recommendation);
}
