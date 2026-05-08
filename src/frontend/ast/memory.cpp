#include "frontend/ast.hpp"

address_of_node_t::address_of_node_t(UP<ast_node_t> &&value)
  : value(std::move(value)) {}
deref_node_t::deref_node_t(UP<ast_node_t> &&value)
  : value(std::move(value)) {}
