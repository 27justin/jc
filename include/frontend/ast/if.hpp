
struct if_node_t : public ast_node_t {
  UP<ast_node_t> condition;
  UP<ast_node_t> pass, reject;
};
