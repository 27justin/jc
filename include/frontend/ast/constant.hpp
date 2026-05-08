#pragma once

#include "frontend/ast.hpp"

class constant_value_node_t : public ast_node_t {
  public:
  constant_value_node_t(token_type_t, const std::string &value);

  token_type_t type;
  std::string  value;
};
