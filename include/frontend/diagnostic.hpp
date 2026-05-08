#pragma once

#include <format>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "frontend/source.hpp"

enum class diagnostic_level_t { eError, eWarn, eInfo };

struct diagnostic_t {
  diagnostic_level_t level;

  enum class code_t {
    syntax_error,
    symbol_not_found,
    member_not_found,
    type_not_found,
    not_a_function,
    not_a_struct,

    incompatible_implicit_cast,
    incompatible_explicit_cast,
    too_few_arguments,
    too_many_arguments,
    argument_type_mismatch,
    unknown_parameter,
    parameter_already_filled,
    branch_type_mismatch,
    non_const_expression,
    infer_error,
    import_not_found,
    address_of_rvalue,
    incompatible_binary_operation,
  } code;

  std::vector<std::string> arguments;

  std::shared_ptr<source_t> source;
  source_location_t         origin; //< What triggered the diagnostic
};

std::string
serialize(const diagnostic_t &);
