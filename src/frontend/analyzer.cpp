#include "frontend/analyzer.hpp"
#include <iostream>
#include <cassert>

void analyze_node(analyzer_t &, node_t &);

void
analyze_block(analyzer_t &analyzer, block_t &block) {
  // Create a new scope, if none yet.
  if (!block.scope) {
    block.scope = std::make_unique<scope_t>();
    block.scope->parent = analyzer.scopes.back();
  }
  analyzer.scopes.push_back(block.scope);

  for (auto &node : block.statements) {
    analyze_node(analyzer, *node);
  }
}

void struct_type_t::compute_memory_layout() {
  uint64_t current_offset = 0;
  alignment = 0;

  for (auto &member : members) {
    uint64_t S = member.type->size;
    uint64_t A = member.type->alignment;

    if (current_offset % A != 0) {
      current_offset += (A - (current_offset % A));
    }

    member.offset = current_offset;

    if (A > alignment) alignment = A;

    current_offset += S;
  }

  // Tail padding
  if (current_offset % alignment != 0) {
    current_offset += (alignment - (current_offset % alignment));
  }
  size = current_offset;
}

void
register_struct(analyzer_t &analyzer, struct_decl_t &struct_) {
  uint64_t struct_size = 0;
  struct_type_t s {};

  // Walk through the struct members to calculate our total size.
  for (auto &member : struct_.members) {

    // First lookup our type.
    auto type = analyzer.types.lookup(member->type->name);
    if (!type) {
      analyzer.error(error_kind_t::eType, member->location, "Unknown type " + member->type->name);
      return;
    }

    if (member->type->is_pointer) {
      type = analyzer.types.get_pointer_to(type, member->type->is_nullable);
    }

    s.members.push_back(struct_member_t {
        .type = type,
        .name = member->identifier,
        .offset = 0
      });
  }

  s.compute_memory_layout();
  struct_.layout = std::make_unique<struct_type_t>(s);
  analyzer.types.add({
      .name = struct_.name,
      .size = struct_.layout->size,
      .alignment = struct_.layout->alignment
    });
}

function_type_t
analyze_function_header(analyzer_t &analyzer, function_header_t &fn) {
  auto return_type = analyzer.types.lookup(fn.return_type->name);
  function_type_t ft;

  if (!return_type) {
    analyzer.error(error_kind_t::eType, fn.location, "Unknown type " + fn.return_type->name);
    return ft;
  }

  std::vector<std::shared_ptr<type_t>> arg_types;

  // Check for pointer
  if (fn.return_type->is_pointer) {
    return_type = analyzer.types.get_pointer_to(return_type, fn.return_type->is_nullable);
  }

  for (auto &param : fn.arguments) {
    // Lookup the type
    auto type = analyzer.types.lookup(param->type->name);
    // If not found, throw type error
    if (!type) {
      analyzer.error(error_kind_t::eType, param->location, "Unknown type " + param->type->name);
      return ft;
    }

    // Check for pointer
    if (param->type->is_pointer) {
      type = analyzer.types.get_pointer_to(type, param->type->is_nullable);
    }

    arg_types.push_back(type);
  }

  auto sym = std::make_shared<symbol_t>(symbol_t{
      .name = fn.name,
      .kind = symbol_kind_t::eFunction,
      .ty = analyzer.types.get_function_type(return_type, arg_types)
    });

  ft = std::get<function_type_t>(sym->ty->detail);
  analyzer.scopes.back()->symbols[fn.name] = sym;
  return ft;
}

void analyze_function(analyzer_t &analyzer, function_decl_t &func) {
  scope_t function_scope;

  auto ft = analyze_function_header(analyzer, *func.header);

  // Add the scope
  for (int i = 0; i < ft.arg_types.size(); ++i) {
    // Register the parameters as variables in our scope
    auto &param = func.header->arguments[i];
    function_scope.symbols[param->identifier] = std::make_shared<symbol_t>(symbol_t {
        .name = param->identifier,
        .kind = symbol_kind_t::eVariable,
        .ty = ft.arg_types[i]
      });
  }

  analyzer.function_stack.push_back(&ft);
  if (func.body) {
    // Analyze the function body
    func.body->scope = std::make_shared<scope_t>(std::move(function_scope));
    func.body->scope->parent = analyzer.scopes.back();
    analyze_block(analyzer, *func.body);
  }
  analyzer.function_stack.pop_back();
}

std::shared_ptr<symbol_t> resolve(analyzer_t &analyzer, node_t &node) {
  if (auto var = dynamic_cast<identifier_expr_t *>(&node)) {
    return analyzer.scopes.back()->resolve(var->name);
  }

  assert(false && "Tried to resolve unsupported node");
  return nullptr;
}

std::shared_ptr<type_t>
type_of(analyzer_t &analyzer, node_t &node) {
  if (dynamic_cast<string_literal_t *>(&node)) {
    return analyzer.types.get_pointer_to(analyzer.types.lookup("u8"), false);
  } else if (dynamic_cast<integer_literal_t *>(&node)) {
    return analyzer.types.lookup("i32");
  } else {
    assert(false && "type_of on unsupported AST node");
  }
}

void analyze_node(analyzer_t &analyzer, node_t &node) {
  auto scope = analyzer.scopes.back();
  if (auto var = dynamic_cast<var_decl_t *>(&node)) {
    auto type = analyzer.types.lookup(var->type->name);
    if (!type) {
      analyzer.error(error_kind_t::eType, var->location, "Unknown type " + var->type->name);
      return;
    }

    if (var->type->is_pointer) {
      type = analyzer.types.get_pointer_to(type, var->type->is_nullable);
    }

    scope->symbols[var->identifier] = std::make_shared<symbol_t>(symbol_t{
        .name = var->identifier,
        .kind = symbol_kind_t::eVariable,
        .ty = type
      });
    return;
  }

  if (auto call = dynamic_cast<call_expr_t *>(&node)) {
    // Lookup the function
    auto fn = resolve(analyzer, *call->callee);

    // Verify that fn is a function
    if (!std::holds_alternative<function_type_t>(fn->ty->detail)) {
      analyzer.error(error_kind_t::eType, call->location, "Type mismatch, got `"+ fn->ty->name +"`, expected function");
      return;
    }

    // Check argument types
    function_type_t ft = std::get<function_type_t>(fn->ty->detail);
    if (ft.arg_types.size() != call->arguments.size()) {
      analyzer.error(error_kind_t::eType, call->location, "Argument count mismatch");
      return;
    }

    for (auto i = 0; i < ft.arg_types.size(); ++i) {
      auto ty = type_of(analyzer, *call->arguments[i]);
      if (ty != ft.arg_types[i]) {
        analyzer.error(error_kind_t::eType, call->arguments[i]->location, "Type mismatch, expected `"+ ft.arg_types[i]->name +"`, got `"+ ty->name +"`");
        return;
      }
    }
    return;
  }

  if (auto ret = dynamic_cast<return_stmt_t *>(&node)) {
    // Get the function frame.
    if (analyzer.function_stack.empty()) {
      analyzer.error(error_kind_t::eScope, ret->location, "Unexpected return, returns are only supported within functions");
      return;
    }

    auto *ft = analyzer.function_stack.back();
    auto ret_type = type_of(analyzer, *ret->expression);

    if (ret_type != ft->return_type) {
      analyzer.error(error_kind_t::eType, ret->expression->location, "Unexpected type `"+ret_type->name+"`, expected `"+ ft->return_type->name +"`");
      return;
    }
    return;
  }

  std::cerr << "Unexpected: " << analyzer.lexer.line(node.location.start.line) << " ("<<node.location.start.column<<")\n";
  assert(false && "Unsupported AST node");
}

void register_extern_sym(analyzer_t &analyzer, extern_decl_t &extern_sym) {
  if (auto fn = dynamic_cast<function_header_t *>(&*extern_sym.symbol)) {
    analyze_function_header(analyzer, *fn);
    return;
  }

  assert(false && "Unhandled `extern` symbol");
}

void
analyzer_t::analyze(program_node_t &program) {
  for (auto &extern_ : program.externs) {
    register_extern_sym(*this, *dynamic_cast<extern_decl_t *>(&*extern_));
  }

  for (auto &struct_ : program.structs) {
    register_struct(*this, *struct_);
  }

  for (auto &decl : program.declarations) {
    if (auto fn = dynamic_cast<function_decl_t *>(&*decl)) {
      analyze_function(*this, *fn);
    }
  }
}

void analyzer_t::error(error_kind_t kind, source_range_t source, const std::string &msg) {
  switch (kind) {
  case error_kind_t::eType:
    std::cerr << "Type error on line " << source.start.line << ":" << source.start.column << "\n";
    break;
  default:
    std::cerr << "Error on line " << source.start.line << ":" << source.start.column << "\n";
  }

  std::string indent(source.start.column, ' ');
  std::cerr << lexer.line(source.start.line) << "\n";
  std::cerr << indent << "^\n";
  std::cerr << indent << msg << "\n";
  std::exit(1);
}


