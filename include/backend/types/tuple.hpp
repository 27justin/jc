#pragma once
#include "backend/type.hpp"

struct tuple_member_t {
  bool                       is_positional;
  std::optional<std::string> name;
  qualified_type_t          *type;
};

class tuple_type_t : public qualified_type_t {
  private:
  std::vector<tuple_member_t> members;

  public:
  tuple_type_t(const std::vector<tuple_member_t> &members);
  ~tuple_type_t();

  const tuple_member_t &
  member_by_position(ssize_t pos) const;
  const tuple_member_t &
  member_by_name(const std::string &) const;

  bool
  equals(const qualified_type_t &) const override;
  ssize_t
  size() const override;
  std::string
  to_string() const override;
};
