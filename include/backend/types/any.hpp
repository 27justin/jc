#include "backend/type.hpp"

struct any_type_t : public qualified_type_t {
  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  std::string
  to_string() const override;

  bool
  castable(cast_mode_t, const qualified_type_t &) const override;
};
