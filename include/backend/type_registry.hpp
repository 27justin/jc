#pragma once

#include "type.hpp"

#include <map>

struct type_registry_t {
  type_registry_t();

  SP<type_t> resolve(const std::string &name);
  SP<qualified_type_t> get_qualified(SP<type_t> base, bool is_ptr, bool is_null, bool is_const);

  SP<type_t> add_builtin(const std::string &name, size_t size, size_t alignment, type_t::kind_t kind);

  SP<type_t> add_function(SP<qualified_type_t> return_type, const std::vector<SP<qualified_type_t>> &arguments, SP<qualified_type_t> receiver, bool is_var_args);

  SP<type_t> add_struct(const std::string &name, struct_layout_t layout);

  SP<type_t> add_alias(const std::string &name, SP<qualified_type_t>, bool is_distinct);
private:
  // The Registry itself
  std::map<std::string, SP<type_t>> registry;

  using qualified_key_t = std::tuple<SP<type_t>, bool, bool, bool>;
  std::map<qualified_key_t, SP<qualified_type_t>> qualified_cache;
};
