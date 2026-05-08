
class tuple_member_node_t : public ast_node_t {
  public:
  tuple_member_node_t(const std::string &, UP<ast_node_t> &&);
  tuple_member_node_t(UP<ast_node_t> &&);

  std::optional<std::string> name;
  UP<ast_node_t>             value;
};

class tuple_value_node_t : public ast_node_t {
  public:
  tuple_value_node_t(std::vector<UP<tuple_member_node_t>> &&);

  std::vector<UP<tuple_member_node_t>> values;
};
