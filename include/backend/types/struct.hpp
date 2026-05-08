#pragma once

#include "backend/type.hpp"

class struct_type_t : public qualified_type_t {
  std::vector<std::pair<std::string, qualified_type_t *>> members;

  public:
  struct_type_t()                      = default;
  ~struct_type_t()                     = default;
  struct_type_t(const struct_type_t &) = delete;
  struct_type_t(struct_type_t &&);

  void
  add(const std::string &member_name, qualified_type_t *type);

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  std::string
  to_string() const override;

  qualified_type_t *
  member_type_by_name(const std::string &) const;
};
