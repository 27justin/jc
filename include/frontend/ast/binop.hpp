#pragma once

struct binop_node_t : public ast_node_t {
  UP<ast_node_t> left, right;
  binop_type_t   op;
};
