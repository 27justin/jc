#pragma once

#include "frontend/ast.hpp"

enum pointer_kind_t { PT_NULLABLE, PT_NON_NULLABLE };

class type_node_t : public ast_node_t {};

class identifier_type_node_t : public type_node_t {
  public:
  UP<path_node_t> path;
};

class slice_type_node_t : public type_node_t {
  public:
  UP<type_node_t> element_type;
  bool            is_mutable;
};

class variable_array_type_node_t : public type_node_t {
  public:
  UP<type_node_t> element_type;
  UP<ast_node_t>  size;
};

class const_array_type_node_t : public type_node_t {
  public:
  UP<type_node_t> element_type;
  ssize_t         size;
};

class pointer_type_node_t : public type_node_t {
  public:
  UP<type_node_t>             element_type;
  std::vector<pointer_kind_t> indirections;
  bool                        is_mutable;
};

struct tuple_type_member_t {
  bool            is_explicit;
  std::string     name;
  UP<type_node_t> type;
};

class tuple_type_node_t : public type_node_t {
  public:
  std::vector<tuple_type_member_t> members;
};

class parameter_node_t;
class function_type_node_t : public type_node_t {
  public:
  function_type_node_t(UP<type_node_t> &&, std::vector<UP<parameter_node_t>> &&);

  UP<type_node_t>                   return_type;
  std::vector<UP<parameter_node_t>> parameters;
};
