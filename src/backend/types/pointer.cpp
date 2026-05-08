#include <sstream>

#include "backend/type.hpp"
#include "backend/types/any.hpp"
#include "backend/types/pointer.hpp"

#include "frontend/ast.hpp"
#include "frontend/ast/type.hpp"

pointer_type_t::pointer_type_t(const std::vector<pointer_kind_t> &indir, const qualified_type_t *el)
  : indirections_(indir)
  , element_type(el) {}

bool
pointer_type_t::equals(const qualified_type_t &o) const {
  if (o.as<pointer_type_t>() == this)
    return true;
  return false;
}

ssize_t
pointer_type_t::size() const {
  return BITSIZE(8);
}

std::string
pointer_type_t::to_string() const {
  std::stringstream ss;

  for (auto &indir : indirections_)
    ss << (indir == PT_NON_NULLABLE ? "!" : "?");
  ss << element_type->to_string();
  return ss.str();
}

bool
pointer_type_t::castable(cast_mode_t mode, const qualified_type_t &o) const {
  if (&o == this)
    return true;

  // We only care about casting to other pointers
  if (!o.is<pointer_type_t>())
    return false;

  auto other = o.as<pointer_type_t>();

  // We allow mismatched indirection only if one side is 'any'
  bool involves_any = base_type()->is<any_type_t>() || other->base_type()->is<any_type_t>();

  if (indirections_.size() != other->indirections_.size() && !involves_any) {
    return false;
  }

  // If not 'any', the base types must be identical (strict pointer typing)
  bool is_same_base = (base_type() == other->base_type());

  if (!is_same_base && !involves_any) {
    return false;
  }

  // Current (this) is the Source, 'other' is the Target.
  auto source_nullability = indirections_.front();
  auto target_nullability = other->indirections_.front();

  // Rule: ? (Nullable) -> ! (Non-Nullable) requires Explicit casting
  if (source_nullability == PT_NULLABLE && target_nullability == PT_NON_NULLABLE) {
    return mode == cast_mode_t::eExplicit;
  }

  // All other combinations are implicitly allowed:
  // ! -> !  (OK)
  // ! -> ?  (OK: Promoting to nullable is safe)
  // ? -> ?  (OK)
  return true;
}

const qualified_type_t *
pointer_type_t::base_type() const {
  return element_type->underlying_type();
}

const std::vector<pointer_kind_t> &
pointer_type_t::indirections() const {
  return indirections_;
}
