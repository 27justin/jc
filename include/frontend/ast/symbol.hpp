#pragma once
#include "frontend/ast.hpp"
class symbol_node_t : public ast_node_t {
  public:
  symbol_node_t(std::unique_ptr<path_node_t> &&path);
  std::unique_ptr<path_node_t> path;
};
