#pragma once

#include "frontend/ast.hpp"

class parameter_node_t : public ast_node_t {
  public:
  parameter_node_t(std::optional<std::string>     name,
                   std::unique_ptr<type_node_t> &&type,
                   bool                           is_mutable);

  std::optional<std::string> name;
  UP<type_node_t>            type;
  bool                       is_mutable;
};

class function_type_node_t;
class function_node_t : public ast_node_t {
  public:
  function_node_t(UP<function_type_node_t> &&, UP<block_node_t> &&body);

  UP<function_type_node_t> function_type;
  UP<block_node_t>         body;
};

struct named_argument_node_t : public ast_node_t {
  public:
  named_argument_node_t(const std::string &name, UP<ast_node_t> &&value);

  std::string    name;
  UP<ast_node_t> value;
};

class function_call_node_t : public ast_node_t {
  public:
  function_call_node_t(UP<ast_node_t> &&callee, std::vector<UP<ast_node_t>> &&arguments);

  UP<ast_node_t>              callee;
  std::vector<UP<ast_node_t>> arguments;
};
