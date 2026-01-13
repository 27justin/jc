#include "frontend/ast.hpp"

#include <iostream>
#include <string>

void dump_ast(const node_t& node, int indent) {
  auto print_indent = [&]() {
    for (int i = 0; i < indent; ++i) std::cout << "  ";
  };

  // Helper to format type paths cleanly
  auto print_type_path = [](const type_stmt_t& ty) {
    if (ty.is_pointer) {
      if (ty.is_nullable) std::cout << "?";
      else std::cout << "!";
    }
    std::cout << ty.name;
  };

  if (auto root = dynamic_cast<const program_node_t *>(&node)) {
    std::cout << "Externs:\n";
    std::cout << "--------\n";
    for (auto &decl : root->externs) {
      dump_ast(*decl, indent);
      std::cout << "\n";
    }

    std::cout << "\nStructs:\n";
    std::cout << "--------\n";
    for (auto &decl : root->structs) {
      dump_ast(*decl, indent);
      std::cout << "\n";
    }

    std::cout << "\nProgram:\n";
    std::cout << "--------\n";
    for (auto &decl : root->declarations) {
      dump_ast(*decl, indent);
      std::cout << "\n";
    }
  }
  else if (auto lit = dynamic_cast<const literal_expr_t*>(&node)) {
    print_indent();
    if (auto str = dynamic_cast<const string_literal_t*>(lit)) std::cout << "[String \"" << str->value << "\"]";
    else if (auto i = dynamic_cast<const integer_literal_t*>(lit)) std::cout << "[Int " << i->value << "]";
    else if (auto f = dynamic_cast<const float_literal_t*>(lit)) std::cout << "[Float " << f->value << "]";
    else if (auto b = dynamic_cast<const bool_literal_t*>(lit)) std::cout << "[Bool " << (b->value ? "true" : "false") << "]";
  }
  else if (auto ident = dynamic_cast<const identifier_expr_t *>(&node)) {
    print_indent();
    std::cout << "[Variable " << ident->name << "]";
  }
  else if (auto acc = dynamic_cast<const member_access_expr_t*>(&node)) {
    print_indent();
    std::cout << "[MemberAccess ." << acc->member_name << " ";
    // We pass 0 for indent here to keep it on the same line
    dump_ast(*acc->object, 0);
    std::cout << "]";
  }
  else if (auto var = dynamic_cast<const var_decl_t*>(&node)) {
    print_indent();
    std::cout << "[VarDecl " << var->identifier << " (Type: ";
    print_type_path(*var->type);
    std::cout << ")";

    if (var->initial_value) {
      std::cout << " (Default: ";
      dump_ast(**var->initial_value, 0);
      std::cout<<")";
    }

    std::cout << "]";
  }
  else if (auto call = dynamic_cast<const call_expr_t*>(&node)) {
    print_indent();
    std::cout << "[Call ";
    // Dump callee on same line
    dump_ast(*call->callee, 0); 
    if (!call->arguments.empty()) {
      std::cout << "\n";
      print_indent();
      std::cout << "  Args:";
      for (const auto& arg : call->arguments) {
        std::cout << "\n";
        dump_ast(*arg, indent + 2);
      }
    }
    std::cout << "]";
  }
  else if (auto ret = dynamic_cast<const return_stmt_t*>(&node)) {
    print_indent();
    std::cout << "[Return ";
    if (ret->expression) dump_ast(*ret->expression, 0);
    std::cout << "]";
  }
  else if (auto block = dynamic_cast<const block_t*>(&node)) {
    print_indent();
    std::cout << "[Block\n";
    for (const auto& stmt : block->statements) {
      dump_ast(*stmt, indent + 1);
      std::cout << "\n";
    }
    print_indent();
    std::cout << "]";
  } else if (auto func = dynamic_cast<const function_header_t *>(&node)) {
    print_indent();
    std::cout << "[Function " << func->name << " (Returns: ";
    print_type_path(*func->return_type);
    std::cout <<  ")\n";

    print_indent();
    std::cout << "  Params:";
    for (const auto& arg : func->arguments) {
      std::cout << "\n";
      dump_ast(*arg, indent + 2);
    }
    std::cout << "\n]";
  }
  else if (auto func = dynamic_cast<const function_decl_t*>(&node)) {
    print_indent();
    std::cout << "[Function " << func->header->name << " (Returns: ";
    print_type_path(*func->header->return_type);
    std::cout <<  ")\n";

    print_indent();
    std::cout << "  Params:";
    for (const auto& arg : func->header->arguments) {
      std::cout << "\n";
      dump_ast(*arg, indent + 2);
    }
    std::cout << "\n";
    if (func->body) dump_ast(*func->body, indent + 1);
    std::cout << "]";
  }
  else if (auto bin = dynamic_cast<const binary_expr_t*>(&node)) {
    print_indent();
    std::cout << "[Binary " << to_text(bin->op) << "\n";

    // Dump Left Side
    if (bin->left) dump_ast(*bin->left, indent + 1);
    std::cout << "\n";

    // Dump Right Side
    if (bin->right) dump_ast(*bin->right, indent + 1);

    std::cout << "]";
  } else if (auto un = dynamic_cast<const unary_expr_t*>(&node)) {
    print_indent();
    std::string op_sym = (un->op == token_type_t::operatorMinus) ? "-" : "!";
    std::cout << "[Unary " << op_sym << "\n";
    if (un->target) dump_ast(*un->target, indent + 1);
    std::cout << "]";
  } else if (auto ext = dynamic_cast<const extern_decl_t *>(&node)) {
    print_indent();
    std::cout << "[Extern [Convention: " << ext->language << "]\n";
    dump_ast(*ext->symbol, indent + 1);
    std::cout << "]";
  } else if (auto decl = dynamic_cast<const struct_decl_t *>(&node)) {
    print_indent();
    std::cout << "[Struct "<< decl->name <<"\n";
    for (auto &var : decl->members) {
      dump_ast(*var, indent + 1);
      std::cout << "\n";
    }
    std::cout << "]";
  }
}
