#pragma once
#include <vector>

class block_node_t : public ast_node_t {
  public:
  block_node_t(std::vector<UP<ast_node_t>> &&statements);

  std::vector<UP<ast_node_t>> statements;
};
