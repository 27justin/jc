
struct for_node_t : public ast_node_t {
  UP<ast_node_t> init;
  UP<ast_node_t> body;
  UP<ast_node_t> action;
};
