
struct uninitialized_node_t : public ast_node_t {};
struct zero_node_t : public ast_node_t {};
struct nil_node_t : public ast_node_t {};

class address_of_node_t : public ast_node_t {
  public:
  address_of_node_t(UP<ast_node_t> &&value);

  UP<ast_node_t> value;
};

class deref_node_t : public ast_node_t {
  public:
  deref_node_t(UP<ast_node_t> &&value);

  UP<ast_node_t> value;
};
