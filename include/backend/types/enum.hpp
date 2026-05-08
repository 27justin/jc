#pragma once

#include "backend/type.hpp"

class enum_type_t : public qualified_type_t {};

// Aggregate enum type, maps to (tag, union)
class aggregate_enum_type_t : public enum_type_t {
  private:
  static constexpr int                                    TAG_SIZE = BYTESIZE(2);
  std::vector<std::pair<std::string, qualified_type_t *>> tags;
  ssize_t                                                 bitsize;

  public:
  aggregate_enum_type_t();
  aggregate_enum_type_t(aggregate_enum_type_t &&);

  void
  add(const std::string &, qualified_type_t *);

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  std::string
  to_string() const override;

  bool
  castable(cast_mode_t, const qualified_type_t &) const override;

  const qualified_type_t *
  tag_by_name(const std::string &) const;
};

// C-style int-like enum
class scalar_enum_type_t : public enum_type_t {
  // TODO: Can't go up to max of uint64_t
  std::map<std::string, ssize_t> members;

  ssize_t
  max() const;

  public:
  scalar_enum_type_t();
  scalar_enum_type_t(scalar_enum_type_t &&);

  void
  add(const std::string &memb, ssize_t value);
  void
  add(const std::string &memb);

  bool
  has_member(const std::string &) const;

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  std::string
  to_string() const override;

  bool
  castable(cast_mode_t, const qualified_type_t &) const override;
};

class variant_constructor_type_t : public qualified_type_t {
  aggregate_enum_type_t  *aggregate_;
  const qualified_type_t *variant_;
  std::string             tag_;

  public:
  variant_constructor_type_t(aggregate_enum_type_t &aggregate, const std::string &tag);

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  std::string
  to_string() const override;

  bool
  castable(cast_mode_t, const qualified_type_t &) const override;

  aggregate_enum_type_t *
  aggregate();
};
