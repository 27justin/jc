#pragma once
#include "backend/type.hpp"

struct number_type_t : public qualified_type_t {};

struct float_type_t : public number_type_t {
  float_type_t(ssize_t bitsize);

  ssize_t bitsize;

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  bool
  castable(cast_mode_t, const qualified_type_t &) const override;

  std::string
  to_string() const override;
};

struct int_type_t : public number_type_t {
  int_type_t(ssize_t bitsize);

  ssize_t bitsize;

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  bool
  castable(cast_mode_t, const qualified_type_t &) const override;

  std::string
  to_string() const override;
};

struct uint_type_t : public number_type_t {
  uint_type_t(ssize_t bitsize);

  ssize_t bitsize;

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  bool
  castable(cast_mode_t, const qualified_type_t &) const override;

  std::string
  to_string() const override;
};

struct bool_type_t : public number_type_t {
  bool_type_t();

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  bool
  castable(cast_mode_t, const qualified_type_t &) const override;

  std::string
  to_string() const override;
};
