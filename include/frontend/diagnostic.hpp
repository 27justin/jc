#pragma once

#include <string>
#include <queue>
#include <format>
#include <optional>

#include "frontend/source.hpp"

enum class diagnostic_level_t {
  eError, eWarn, eInfo
};

struct diagnostic_t {
  diagnostic_level_t level;
  std::string message, //< Syntax error, erc.
    detail, // Expected X got Y
    suggestion; // Did you mean ...

  const source_t &source;
  std::optional<source_location_t> origin; //< What triggered the diagnostic
};

struct diagnostic_stack_t {
  std::queue<diagnostic_t> messages;
};

template<typename ...FormatArgs>
std::string fmt(const std::string_view literal, FormatArgs &&...args) {
  return std::format(literal, args...);
}

diagnostic_t warn(const source_t &,
                  source_location_t,
                  const std::string &message,
                  const std::string &detail,
                  const std::string &suggestion);

diagnostic_t warn(const source_t &source,
                  const std::string &message,
                  std::string detail = "",
                  std::string suggestion = "");
