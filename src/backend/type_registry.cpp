#include "backend/type_registry.hpp"
#include "backend/type.hpp"

#include <sstream>
#include <format>
#include <memory>

type_registry_t::type_registry_t() {
  add_builtin("void", 0, 0, type_kind_t::eVoid);

  // Only allowed as pointer
  add_builtin("any", 0, 0, type_kind_t::eVoid);

  // Integers
  add_builtin("i8", 1 * 8, 1, type_kind_t::eInt);
  add_builtin("u8", 1 * 8, 1, type_kind_t::eUint);

  add_builtin("i16", 2 * 8, 2, type_kind_t::eInt);
  add_builtin("u16", 2 * 8, 2, type_kind_t::eUint);

  add_builtin("i32", 4 * 8, 4, type_kind_t::eInt);
  add_builtin("u32", 4 * 8, 4, type_kind_t::eUint);

  add_builtin("i64", 8 * 8, 8, type_kind_t::eInt);
  add_builtin("u64", 8 * 8, 8, type_kind_t::eUint);

  // Floats
  add_builtin("f32", 4 * 8, 4, type_kind_t::eFloat);
  add_builtin("f64", 8 * 8, 8, type_kind_t::eFloat);

  // Bool
  add_builtin("bool", 1, 1, type_kind_t::eBool);
}

SP<type_t> type_registry_t::resolve(const std::string &name) {
  if (registry.contains(name))
    return registry.at(name);
  return nullptr;
}

SP<type_t> type_registry_t::add_builtin(const std::string &name,
                                        size_t size,
                                        size_t alignment,
                                        type_kind_t kind) {
  auto t = std::make_shared<type_t>();
  t->name = name;
  t->size = size;
  t->alignment = alignment;
  t->kind = kind;
  registry[name] = t;
  return t;
}

SP<type_t> type_registry_t::add_function(
    SP<type_t> return_type,
    const std::vector<SP<type_t>> &arguments,
    SP<type_t> receiver,
    bool is_var_args) {
  auto t = std::make_shared<type_t>();

  // Signature to later validate calls.
  auto signature = new function_signature_t;
  signature->arg_types = arguments;
  signature->return_type = return_type;
  signature->receiver = receiver;
  signature->is_var_args = is_var_args;

  t->kind = type_kind_t::eFunction;
  t->as.function = signature;

  // Serialize into a name
  std::stringstream ss;
  ss << "fn <" << to_string(return_type) << "> (";

  for (auto i = 0; i < arguments.size(); i++) {
    ss << to_string(arguments[i]);
    if (i < arguments.size() - 1) ss << ", ";
  }

  ss << ")";

  // Set the name
  t->name = ss.str();

  // Use zero size for type, this can't be allocated anyhow
  t->size = 0;
  t->alignment = 0;

  registry[t->name] = t;
  return t;
}

SP<type_t> type_registry_t::add_struct(const std::string &name,
                                       struct_layout_t layout) {
  auto type = std::make_shared<type_t>();
  type->size = layout.size;
  type->alignment = layout.alignment;

  type->kind = type_kind_t::eStruct;
  type->as.struct_layout = new struct_layout_t(std::move(layout));
  type->name = name;

  registry[name] = type;
  return type;
}

SP<type_t> type_registry_t::add_alias(const std::string &name,
                                      SP<type_t> alias,
                                      bool is_distinct) {
  auto type = std::make_shared<type_t>();
  type->size = size_of(alias);
  type->alignment = alignment_of(alias);

  type_alias_t *alias_decl = new type_alias_t {};
  alias_decl->alias = alias;

  type->kind = is_distinct ? type_kind_t::eOpaque : type_kind_t::eAlias;
  type->as.alias = alias_decl;
  type->name = name;

  registry[name] = type;
  return type;
}

SP<type_t> type_registry_t::pointer_to(SP<type_t> base,
                                       std::vector<pointer_kind_t> indirections,
                                       bool is_mutable) {
  auto type = std::make_shared<type_t>();
  type->size = sizeof(void*);
  type->alignment = sizeof(void*);

  pointer_t *ptr = new pointer_t {};
  ptr->indirections = indirections;
  ptr->is_mutable = is_mutable;
  ptr->base = base;

  type->kind = type_kind_t::ePointer;
  type->as.pointer = ptr;
  type->name = base->name;

  registry[to_string(type)] = type;
  return type;
}
