#include "frontend/diagnostic.hpp"
#include <sstream>

diagnostic_t diagnostic(
  diagnostic_level_t level,
  std::shared_ptr<source_t> source,
  const std::string &message,
  const std::string &detail,
  const std::string &suggestion,
  source_location_t origin) {

  return diagnostic_t {
    .level = level,
    .message = message,
    .detail = detail,
    .suggestion = suggestion,
    .source = source,
    .origin = origin
  };
}

diagnostic_t warn(std::shared_ptr<source_t> source,
                  source_location_t loc,
  const std::string &message,
  std::string detail,
  std::string suggestion) {
  return diagnostic(diagnostic_level_t::eWarn, source, message, detail, suggestion, loc);
}

diagnostic_t error(std::shared_ptr<source_t> source,
                   source_location_t loc,
  const std::string &message,
  std::string detail,
  std::string suggestion) {
  return diagnostic(diagnostic_level_t::eError, source, message, detail, suggestion, loc);
}

#define ANSI_BOLD "\u001b[1m"
#define ANSI_ITALIC "\u001b[3m"
#define ANSI_UNDERLINE "\u001b[4m"
#define ANSI_RESET "\x1b[0m"

#define ANSI_RED "\x1b[31m"
#define ANSI_GREEN "\x1b[32m"
#define ANSI_ORANGE "\x1b[33m"
#define ANSI_BLUE "\x1b[34m"

std::string serialize(const diagnostic_t &msg) {
  std::stringstream ss;

  auto offset = msg.origin.start.column + msg.source->name().size() + 2;

  std::string indent = std::string(msg.origin.start.column, ' ');

  auto repeat = [](std::string str, size_t repeat) {
    std::stringstream ss;
    for (size_t i = 0; i < repeat; ++i)
      ss << str;
    return ss.str();
  };

  ss << msg.source->name() << ":" <<msg.origin.start.line << ":" << msg.origin.start.column << ": " ANSI_BOLD;
  switch (msg.level) {
  case diagnostic_level_t::eError:
    ss << ANSI_RED << "error:";
    break;
  case diagnostic_level_t::eWarn:
    ss << ANSI_ORANGE << "warning:";
    break;
  case diagnostic_level_t::eInfo:
    ss << ANSI_BLUE << "info:";
    break;
  }
  ss << ANSI_RESET << " " << msg.message << "\n\n";

  auto line = msg.source->line(msg.origin.start.line);
  ss << line.substr(0, msg.origin.start.column);
  ss << "\e[0;91m" << line.substr(msg.origin.start.column, msg.origin.end.column - msg.origin.start.column);
  ss << "\e[0m" << line.substr(msg.origin.end.column) << "\n";

  if (msg.detail.size() > 0) {
    ss << indent << "├" << repeat("─", msg.detail.size() + 2) << "┐\n";
    ss << indent << "│ " << msg.detail << " │\n";
    ss << indent << "└" << repeat("─", msg.detail.size() + 2) << "┘\n";
  }

  if (msg.suggestion.size() > 0) {
    ss << indent << "  " << msg.suggestion << "\n";
  }

  return ss.str();
}
