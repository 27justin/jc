#pragma once

#include <map>
#include <string>
#include <vector>

#include "type.hpp"

struct type_registry_t {
  std::map<std::string, std::shared_ptr<type_t>> types;

  std::shared_ptr<type_t>
  add(type_t &&decl, const std::string &alias);

  std::shared_ptr<type_t>
  add(type_t &&decl);

  std::shared_ptr<type_t>
  add_primitive(const std::string &nane, uint64_t size, uint64_t alignment);

  std::shared_ptr<type_t>
  lookup(const std::string &name);

  void
  alias(const std::string &from, const std::string &to);

  std::shared_ptr<type_t>
  get_pointer_to(std::shared_ptr<type_t>, bool);

  std::shared_ptr<type_t>
  get_function_type(std::shared_ptr<type_t>, std::vector<std::shared_ptr<type_t>>);

  type_registry_t();
};
