
class enum_variant_node_t : public ast_node_t {
  public:
  enum_variant_node_t(const std::string &, std::vector<UP<parameter_node_t>> &&);
  enum_variant_node_t(const std::string &, UP<ast_node_t> &&);

  std::string                       identifier;
  std::vector<UP<parameter_node_t>> fields;

  // Explicit value assignment
  UP<ast_node_t> explicit_tag_value;
};

class enum_definition_node_t : public ast_node_t {
  public:
  enum_definition_node_t(std::vector<UP<enum_variant_node_t>> &&);

  std::vector<UP<enum_variant_node_t>> variants;
};
