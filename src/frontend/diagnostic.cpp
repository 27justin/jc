#include "frontend/diagnostic.hpp"
#include <iomanip>
#include <sstream>

#define ANSI_BOLD      "\u001b[1m"
#define ANSI_ITALIC    "\u001b[3m"
#define ANSI_UNDERLINE "\u001b[4m"
#define ANSI_RESET     "\x1b[0m"

#define ANSI_RED    "\x1b[31m"
#define ANSI_GREEN  "\x1b[32m"
#define ANSI_ORANGE "\x1b[33m"
#define ANSI_BLUE   "\x1b[34m"

std::string
get_error_string(diagnostic_t::code_t code, const std::vector<std::string> &args) {
  std::string format;

  using DT = diagnostic_t::code_t;
  switch (code) {
    case DT::syntax_error:
      format = "expected {} but found {}";
      break;
    case DT::symbol_not_found:
      format = "use of undeclared identifier '{}'";
      break;
    case DT::member_not_found:
      format = "struct '{}' has no member named '{}'";
      break;
    case DT::type_not_found:
      format = "unknown type name '{}'";
      break;
    case DT::not_a_function:
      format = "called object of type '{}' is not a function";
      break;
    case DT::not_a_struct:
      format = "cannot initialize non-aggregate type '{}' with an initializer list";
      break;
    case DT::incompatible_implicit_cast:
      format = "no implicit conversion from '{}' to '{}'";
      break;
    case DT::incompatible_explicit_cast:
      format = "operand types are incompatible ('{}' and '{}')";
      break;
    case DT::too_few_arguments:
      format = "too few arguments to function; expected {}, have {}";
      break;
    case DT::too_many_arguments:
      format = "too many arguments to function; expected {}, have {}";
      break;
    case DT::argument_type_mismatch:
      format = "cannot initialize parameter '{}' of type '{}' with an value of type '{}'";
      break;
    case DT::unknown_parameter:
      format = "named parameter '{}' is not known on this function";
      break;
    case DT::parameter_already_filled:
      format = "named parameter '{}' was previously filled";
      break;
    case DT::branch_type_mismatch:
      format = "else type '{}' is not implicitly convertible to '{}'";
      break;
    case DT::non_const_expression:
      format = "this expression is not constant at compile time and can't be evaluated";
      break;
    case DT::infer_error:
      format = "failed to infer what this refers to, try a fully qualified path";
      break;
    case DT::import_not_found:
      format = "import '{}' was not found";
      break;
    case DT::address_of_rvalue:
      format = "taking the address of a temporary value is not permitted";
      break;
    case DT::incompatible_binary_operation:
      format = "binary operation {} is not supported on operands '{}' and '{}'";
      break;
    default:
      return "unknown compiler error";
  }

  for (const auto &arg : args) {
    size_t pos = format.find("{}");
    if (pos != std::string::npos) {
      format.replace(pos, 2, arg);
    }
  }
  return format;
}

std::string
serialize(const diagnostic_t &msg) {
  std::stringstream ss;

  ss << ANSI_BOLD << msg.source->name() << ":" << msg.origin.start.line << ":"
     << msg.origin.start.column << ": ";

  switch (msg.level) {
    case diagnostic_level_t::eError:
      ss << ANSI_RED << "error: ";
      break;
    case diagnostic_level_t::eWarn:
      ss << ANSI_ORANGE << "warning: ";
      break;
    case diagnostic_level_t::eInfo:
      ss << ANSI_BLUE << "info: ";
      break;
  }

  ss << ANSI_RESET << get_error_string(msg.code, msg.arguments) << "\n";
  if (msg.origin.start.line > 0) {
    int         gutter_width = std::to_string(msg.origin.end.line).length();
    std::string padding(gutter_width, ' ');

    ss << padding << " |\n"; // Top
    for (size_t i = msg.origin.start.line; i <= msg.origin.end.line; ++i) {
      std::string line = std::string(msg.source->line(i));

      ss << std::setw(gutter_width) << i << " | ";
      size_t col_start = (i == msg.origin.start.line) ? msg.origin.start.column : 0;
      size_t col_end   = (i == msg.origin.end.line) ? msg.origin.end.column : line.size();

      ss << line.substr(0, col_start) << ANSI_RED << line.substr(col_start, col_end - col_start)
         << ANSI_RESET << line.substr(col_end) << "\n";

      if (i == msg.origin.start.line) {
        ss << padding << " | " << std::string(col_start, ' ') << ANSI_RED;
        size_t underline_len = (msg.origin.start.line == msg.origin.end.line)
                               ? (msg.origin.end.column - msg.origin.start.column)
                               : (line.size() - col_start);

        ss << "^" << std::string(underline_len > 0 ? underline_len - 1 : 0, '~') << ANSI_RESET;
        ss << "\n";
      }
    }
    ss << padding << " |\n"; // Trailing bottom
  }

  return ss.str();
}
