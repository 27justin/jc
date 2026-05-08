#pragma once
#include "backend/type.hpp"

class nominal_type_t : public qualified_type_t {
  public:
  nominal_type_t(const std::string &name, qualified_type_t *base);

  std::string       name;
  qualified_type_t *base;

  bool
  equals(const qualified_type_t &) const override;
  ssize_t
  size() const override;
  std::string
  to_string() const override;

  bool
  castable(cast_mode_t, const qualified_type_t &) const override;

  const qualified_type_t *
  underlying_type() const override;
};

struct alias_type_t : public qualified_type_t {
  qualified_type_t *nominal;

  bool
  equals(const qualified_type_t &) const override;
  ssize_t
  size() const override;
  std::string
  to_string() const override;

  const qualified_type_t *
  underlying_type() const override;
};
