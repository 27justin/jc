#include "frontend/ast.hpp"

constant_value_node_t::constant_value_node_t(token_type_t type, const std::string &value)
  : type(type)
  , value(value) {}
