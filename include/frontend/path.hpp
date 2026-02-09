#pragma once

#include <string>
#include <vector>
#include <memory>

struct constraint_t {
  std::string name;
};

struct path_t;
struct generic_t;
struct type_decl_t;

struct path_element_t {
  std::string name;
  std::vector<generic_t> generic_args;

  bool operator==(const path_element_t &other) const;
};

struct path_t {
  std::vector<path_element_t> segments;

  bool has_generic() const;
  bool operator==(const path_t &other) const;
};

struct generic_t {
  std::shared_ptr<type_decl_t> binding; //< type "name" within the scope (e.g. T, etc.)
  std::vector<path_t> constraints; //< constraint, e.g. `i32`, or contracts., etc.

  bool operator==(const generic_t &other) const;
};

std::string to_string(const path_t &);
std::string to_string(const generic_t &);
