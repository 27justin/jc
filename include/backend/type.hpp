#pragma once

#include <concepts>
#include <map>
#include <memory>
#include <optional>
#include <unordered_map>
#include <vector>

template<typename T>
using SP = std::shared_ptr<T>;

enum class cast_mode_t { eImplicit, eExplicit };

class qualified_type_t {
  protected:
  qualified_type_t() = default;

  public:
  virtual ~qualified_type_t()                = default;
  qualified_type_t(const qualified_type_t &) = delete;
  qualified_type_t(qualified_type_t &&)      = delete;

  virtual bool
  equals(const qualified_type_t &) const = 0;

  virtual ssize_t
  size() const = 0;

  virtual std::string
  to_string() const = 0;

  virtual bool
  castable(cast_mode_t, const qualified_type_t &) const;

  virtual const qualified_type_t *
  underlying_type() const;

  template<typename _Derived>
    requires std::derived_from<_Derived, qualified_type_t>
  const _Derived *
  as() const {
    return dynamic_cast<const _Derived *>(this->underlying_type());
  }

  template<typename _Derived>
    requires std::derived_from<_Derived, qualified_type_t>
  _Derived *
  as() {
    return dynamic_cast<_Derived *>(const_cast<qualified_type_t *>(this->underlying_type()));
  }

  template<typename _Derived>
    requires std::derived_from<_Derived, qualified_type_t>
  bool
  is() const {
    return dynamic_cast<const _Derived *>(this->underlying_type()) != nullptr;
  }

  void
  operator=(const qualified_type_t &) = delete;

  void
  operator=(const qualified_type_t &&) = delete;
};

#define BYTESIZE(num) (num * 8)
#define BITSIZE(num)  (num)
