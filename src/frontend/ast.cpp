#include "frontend/ast.hpp"

#include <iostream>
#include <string>

void dump_ast(ast_node_t &node, size_t indent_val) {
  auto indent = [indent_val]() {
    return std::string(indent_val * 2, ' ');
  };

  std::cout << indent();
  switch (node.type) {
  case ast_node_t::eIf: {
    if_stmt_t *stmt = node.as.if_stmt;
    std::cout << "[If ";
    dump_ast(*stmt->condition, 0);
    std::cout << "\n";
    dump_ast(*stmt->pass, indent_val + 1);
    if (stmt->reject) {
      std::cout << "\n";
      dump_ast(*stmt->reject, indent_val + 1);
    }
    return;
  }
  case ast_node_t::eAddrOf: {
    addr_of_expr_t *addr = node.as.addr_of;
    std::cout << "[Address ";
    dump_ast(*addr->value, 0);
    std::cout << "]";
    return;
  }
  case ast_node_t::eMemberAccess: {
    member_access_expr_t *expr = node.as.member_access;
    std::cout << "[MemberAccess "; dump_ast(*expr->object, 0); std::cout <<"."<< expr->member <<"]";
    return;
  }
  case ast_node_t::eReturn: {
    return_stmt_t &ret = *node.as.return_stmt;
    std::cout << "[Return ";
    dump_ast(*ret.value);
    std::cout << "]\n";
    return;
  };
  case ast_node_t::eCall: {
    call_expr_t &call = *node.as.call_expr;

    std::cout << "[Call ";
    dump_ast(*call.callee);
    std::cout << " with (";

    for (auto &v : call.arguments) {
      dump_ast(*v);
      std::cout << " ";
    }

    std::cout << ")]";
    return;
  }
  case ast_node_t::eUnary: {
    unary_expr_t &expr = *node.as.unary;
    std::cout << "[Unary " << to_text(expr.op);
    dump_ast(*expr.value);
    std::cout << "]";
    return;
  }
  case ast_node_t::eBinop: {
    binop_expr_t &expr = *node.as.binop;
    std::cout << "[Binary Operation " << to_text(expr.op) << "\n";
    dump_ast(*expr.left, indent_val + 1);
    std::cout << "\n";
    dump_ast(*expr.right, indent_val + 1);
    std::cout << "\n";
    std::cout << indent() << "]\n";
    return;
  }
  case ast_node_t::eBlock: {
    block_node_t &block = *node.as.block;

    std::cout << "[Block\n";
    for (auto &v : block.body) {
      dump_ast(*v, indent_val + 1);
    }
    std::cout << "]";
    return;
  }
  case ast_node_t::eLiteral: {
    literal_expr_t &lit = *node.as.literal_expr;
    std::cout << "[Literal <";
    switch (lit.type) {
    case literal_type_t::eBool:
      std::cout << "bool";
      break;
    case literal_type_t::eInteger:
      std::cout << "int?";
      break;
    case literal_type_t::eFloat:
      std::cout << "f32";
      break;
    case literal_type_t::eString:
      std::cout << "!u8";
      break;
    }
    std::cout << "> \"" << lit.value << "\"]";
    return;
  }
  case ast_node_t::eExtern: {
    auto &decl = *node.as.extern_decl;
    std::cout << "[Extern " << decl.convention << "\n";
    dump_ast(*decl.import, indent_val + 1);
    std::cout << indent() << "]\n";
    return;
  }
  case ast_node_t::eSymbol: {
    symbol_expr_t &lookup = *node.as.symbol;
    // Type or variable
    std::cout << "[Symbol " << lookup.identifier << "]";
    return;
  }
  case ast_node_t::eDeclaration: {
    declaration_t &decl = *node.as.declaration;

    std::cout << "[Declare ";
    std::cout << decl.identifier;
    std::cout << " <";
    if (decl.is_mutable) {
      std::cout << "var ";
    }
    if (decl.type)
      dump_ast(*decl.type);
    else
      std::cout << "infer";
    std::cout << ">";

    if (decl.value) {
      std::cout << " "; dump_ast(*decl.value);
    }
    std::cout << "\n";
    return;
  }
  case ast_node_t::eType: {
    ast_type_t &ty = *node.as.type;

    if (ty.is_pointer && ty.is_nullable)
      std::cout << '?';
    else if (ty.is_pointer)
      std::cout << '!';
    std::cout << ty.name;
    return;
  }
  case ast_node_t::eFunctionImpl: {
    auto &impl = *node.as.fn_impl;
    std::cout << "[Function (Signature: "; dump_ast(*impl.declaration, 0); std::cout <<")\n";

    dump_ast(*impl.block, indent_val + 1);

    std::cout << "]\n";
    return;
  }
  case ast_node_t::eFunctionDecl: {
    auto &decl = *node.as.fn_decl;
    std::cout << "fn <"; dump_ast(*decl.type); std::cout << "> ";
    std::cout << decl.name << "(";
    for (auto i = 0; i < decl.parameters.size(); ++i) {
      dump_ast(*decl.parameters[i]);
      if (i < decl.parameters.size() - 1) std::cout << ", ";
    }
    std::cout << ")";
    return;
  }
  default:
    std::cerr << "<Unhandled dump_ast node type: " << (int)node.type << ">\n";
    return;
  }
}
