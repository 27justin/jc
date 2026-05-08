#include "backend/types/function.hpp"
#include "backend/type.hpp"
#include <sstream>

function_type_t::function_type_t(qualified_type_t                      *return_type,
                                 const std::vector<qualified_type_t *> &params)
  : return_type(return_type)
  , parameters(params) {}

bool
function_type_t::equals(const qualified_type_t &ty) const {
  if (ty.is<function_type_t>() == false)
    return false;

  auto other = ty.as<function_type_t>();
  if (!other->return_type->equals(*return_type))
    return false;
  if (other->parameters.size() != parameters.size())
    return false;

  for (auto i = 0; i < parameters.size(); i++) {
    if (other->parameters[i]->equals(*parameters[i]) == false)
      return false;
  }

  return true;
}

ssize_t
function_type_t::size() const {
  return BYTESIZE(8);
}

std::string
function_type_t::to_string() const {
  std::stringstream ss;
  ss << "fn (";

  for (auto i = 0; i < parameters.size(); i++) {
    ss << parameters[i]->to_string();
    if (i < parameters.size() - 1)
      ss << ", ";
  }

  ss << ") -> " << return_type->to_string();
  return ss.str();
}

annotated_function_type_t::annotated_function_type_t(
  qualified_type_t                      *return_type,
  const std::vector<qualified_type_t *> &parameters,
  const std::vector<std::string>        &labels)
  : function_type_t(return_type, parameters)
  , labels(labels) {}

bool
annotated_function_type_t::equals(const qualified_type_t &ty) const {
  if (this == &ty)
    return true;
  return function_type_t::equals(ty);
}

bool
annotated_function_type_t::castable(cast_mode_t mode, const qualified_type_t &ty) const {
  return function_type_t::castable(mode, ty);
}

const qualified_type_t *
annotated_function_type_t::underlying_type() const {
  return static_cast<const function_type_t *>(this);
}

std::string
annotated_function_type_t::to_string() const {
  std::stringstream ss;
  ss << "fn (";

  for (auto i = 0; i < parameters.size(); i++) {
    ss << labels[i] << ": ";
    ss << parameters[i]->to_string();
    if (i < parameters.size() - 1)
      ss << ", ";
  }

  ss << ") -> " << return_type->to_string();
  return ss.str();
}

std::string
annotated_function_type_t::label_by_index(ssize_t index) const {
  if (index < 0 && index >= labels.size())
    return "";
  return labels[index];
}

ssize_t
annotated_function_type_t::index_by_label(const std::string &search) const {
  ssize_t index = 0;
  for (auto &label : labels) {
    if (search == label)
      return index;
    index++;
  }
  return -1;
}
