#include "frontend/type_registry.hpp"
#include "frontend/symbol.hpp"
#include <sstream>

std::shared_ptr<type_t>
type_registry_t::add(type_t &&decl) {
  std::string name = decl.name;
  types[name] = std::make_unique<type_t>(std::move(decl));
  return types.at(name);
}

std::shared_ptr<type_t>
type_registry_t::add(type_t &&decl, const std::string &alias) {
  types[alias] = std::make_unique<type_t>(std::move(decl));
  return types.at(alias);
}

std::shared_ptr<type_t>
type_registry_t::add_primitive(const std::string &name, uint64_t size, uint64_t alignment) {
  return add({
      .name = name,
      .size = size,
      .alignment = alignment
    });
}

void
type_registry_t::alias(const std::string &from, const std::string &to) {
  types[to] = types[from];
}

std::shared_ptr<type_t>
type_registry_t::lookup(const std::string &name) {
  if (types.contains(name))
    return types.at(name);
  return nullptr;
}

std::shared_ptr<type_t> type_registry_t::get_pointer_to(std::shared_ptr<type_t> base,
                                                        bool nullable) {
  if (nullable) {
    if (auto t = lookup("?" + base->name))
      return t;

    return add({
        .name = "?" + base->name,
        .size = 8,
        .alignment = 8
      });
  } else {
    if (auto t = lookup("!" + base->name))
      return t;

    return add({
        .name = "!" + base->name,
        .size = 8,
        .alignment = 8
      });
  }
}

std::shared_ptr<type_t>
type_registry_t::get_function_type(std::shared_ptr<type_t> return_type,
                                   std::vector<std::shared_ptr<type_t>> arg_types) {
  std::stringstream ss;
  ss << "fn ";
  ss << "<" << return_type->name << "> ";
  ss << "(";

  for (auto i = 0; i < arg_types.size(); ++i) {
    ss << arg_types[i]->name;
    if (i < arg_types.size() - 1) ss << ", ";
  }

  function_type_t ft {
    .return_type = return_type,
    .arg_types = arg_types
  };

  return add(type_t{
      .name = ss.str(),
      .size = 8,
      .alignment = 8,
      .detail = ft
    });
}

type_registry_t::type_registry_t() {
  // Seed our primitives
  add_primitive("u8", 1, 1);
  add_primitive("i8", 1, 1);

  add_primitive("u16", 2, 2);
  add_primitive("i16", 2, 2);

  add_primitive("u32", 4, 4);
  add_primitive("i32", 4, 4);

  add_primitive("u64", 8, 8);
  add_primitive("i64", 8, 8);

  add_primitive("bool", 1, 1);

  add_primitive("void", 0, 0);
}

