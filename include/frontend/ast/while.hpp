
struct while_node_t : public ast_node_t {
  UP<ast_node_t> condition;
  UP<ast_node_t> body;
};

struct do_while_node_t : public ast_node_t {
  UP<ast_node_t> body;
  UP<ast_node_t> condition;
};
