#pragma once

#include "backend/type.hpp"
#include "frontend/ast/type.hpp"

class slice_type_t : public qualified_type_t {
  bool              is_mutable;
  qualified_type_t *element_type;

  public:
  slice_type_t(bool, qualified_type_t *);

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  std::string
  to_string() const override;
};
