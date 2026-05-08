#pragma once
#include <optional>

class declaration_node_t : public ast_node_t {
  public:
  declaration_node_t(UP<ast_node_t> &&);

  enum mutability { var, let };
  mutability storage_class;

  UP<ast_node_t>  where;
  UP<ast_node_t>  value;
  UP<type_node_t> declared_type;

  void
  visit(const std::function<void(ast_node_t &)> &visitor) override;
};
