#pragma once

#include "backend/type.hpp"
#include "frontend/ast/type.hpp"

class pointer_type_t : public qualified_type_t {
  std::vector<pointer_kind_t> indirections_;
  const qualified_type_t     *element_type;

  public:
  pointer_type_t(const std::vector<pointer_kind_t> &, const qualified_type_t *);

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  std::string
  to_string() const override;

  bool
  castable(cast_mode_t, const qualified_type_t &) const override;

  const qualified_type_t *
  base_type() const;

  const std::vector<pointer_kind_t> &
  indirections() const;
};
