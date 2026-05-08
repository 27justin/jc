
class result_check_node_t : public ast_node_t {
  public:
  result_check_node_t(UP<ast_node_t> &&expression,
                      UP<ast_node_t> &&expected,
                      UP<ast_node_t> &&fallback);

  // world.create(100) is _.size() == 100 or panic();
  UP<ast_node_t> expression; // The expression `world.create(100)`
  UP<ast_node_t> expected;   // The boolean expression `_.size() == 100`
  UP<ast_node_t> fallback;   // The panic() call
};
