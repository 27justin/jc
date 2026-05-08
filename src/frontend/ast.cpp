#include "frontend/ast.hpp"
#include "backend/type.hpp"

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

ast_node_t::~ast_node_t() {}

ast_node_t::ast_node_t(ast_node_t &&) {}
ast_node_t::ast_node_t() {}

void
ast_node_t::visit(const std::function<void(ast_node_t &)>       &visitor,
                  const std::function<bool(const ast_node_t &)> &filter) {
  visit([filter, visitor](ast_node_t &node) {
    if (filter(node))
      visitor(node);
  });
}

void
ast_node_t::visit(const std::function<void(ast_node_t &)> &visitor) {
  visitor(*this);
}

std::string
to_string(binop_type_t ty) {
  switch (ty) {
    case binop_type_t::eAdd:
      return "+";
    case binop_type_t::eSubtract:
      return "-";
    case binop_type_t::eDivide:
      return "/";
    case binop_type_t::eMultiply:
      return "*";
    case binop_type_t::eAnd:
      return "&&";
    case binop_type_t::eOr:
      return "||";
    case binop_type_t::eEqual:
      return "==";
    case binop_type_t::eNotEqual:
      return "!=";
    case binop_type_t::eGT:
      return ">";
    case binop_type_t::eGTE:
      return ">=";
    case binop_type_t::eLT:
      return "<";
    case binop_type_t::eLTE:
      return "<=";
    case binop_type_t::eMod:
      return "%";
    case binop_type_t::eBitAnd:
      return "&";
    case binop_type_t::eBitOr:
      return "|";
    case binop_type_t::eBitShiftLeft:
      return "<<";
    case binop_type_t::eBitShiftRight:
      return ">>";
    case binop_type_t::eXor:
      return "^";
    default:
      assert(false && "Missing to_string(binop_type_t) case");
  }
  return "";
}
