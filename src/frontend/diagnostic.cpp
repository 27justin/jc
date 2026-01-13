#include "frontend/diagnostic.hpp"

diagnostic_t diagnostic(
  diagnostic_level_t level,
  const source_t &source,
  const std::string &message,
  const std::string &detail,
  const std::string &suggestion,
  std::optional<source_location_t> origin = std::nullopt) {

  return diagnostic_t {
    .level = level,
    .message = message,
    .detail = detail,
    .suggestion = suggestion,
    .source = source,
    .origin = origin
  };
}

diagnostic_t warn(const source_t &source, source_location_t loc,
                  const std::string &message, const std::string &detail,
                  const std::string &suggestion) {
  return diagnostic(diagnostic_level_t::eWarn, source, message, detail, suggestion, loc);
}

diagnostic_t warn(
  const source_t &source,
  const std::string &message,
  std::string detail,
  std::string suggestion) {
  return diagnostic(diagnostic_level_t::eWarn, source, message, detail, suggestion);
}

