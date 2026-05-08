struct struct_member_node_t : public ast_node_t {
  std::string     name;
  UP<type_node_t> type;
};

struct struct_definition_node_t : public ast_node_t {
  std::vector<UP<struct_member_node_t>> members;
};

struct struct_init_node_t : public ast_node_t {
  UP<ast_node_t> target_type;

  struct field_val {
    std::string    name;
    UP<ast_node_t> value;
  };
  std::vector<field_val> fields;
};
