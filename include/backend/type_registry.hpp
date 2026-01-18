#pragma once

#include "type.hpp"

#include <map>

struct type_registry_t {
  type_registry_t();

  SP<type_t> resolve(const std::string &name);

  SP<type_t> add_builtin(const std::string &name, size_t size, size_t alignment, type_kind_t kind);
  SP<type_t> add_function(SP<type_t> return_type, const std::vector<SP<type_t>> &arguments, SP<type_t> receiver, bool is_var_args);
  SP<type_t> add_struct(const std::string &name, struct_layout_t layout);
  SP<type_t> add_alias(const std::string &name, SP<type_t>, bool is_distinct);

  SP<type_t> pointer_to(SP<type_t> base, std::vector<pointer_kind_t> indirections, bool is_mutable);
  SP<type_t> array_of(SP<type_t> base, size_t len);
  SP<type_t> slice_of(SP<type_t> base, bool is_mutable);
private:
  std::map<std::string, SP<type_t>> registry;
};
