#include "backend/symbol.hpp"
#include "backend/scope.hpp"
#include "backend/type.hpp"

#include <memory>

scope_t::scope_t(SP<scope_t> parent)
    : parent(parent)
    , types(parent ? &parent->types : nullptr) {
}

void scope_t::merge(const scope_t &other) {
  symbols.insert(other.symbols.begin(), other.symbols.end());
  templates.insert(templates.end(), other.templates.begin(), other.templates.end());
  types.merge(other.types);
}

SP<symbol_t> scope_t::resolve(const path_t &identifier) {
  if (symbols.contains(to_string(identifier)))
    return symbols.at(to_string(identifier));

  if (parent)
    return parent->resolve(identifier);

  return nullptr;
}

void scope_t::remove(const path_t &identifier) {
  if (symbols.contains(to_string(identifier))) {
    symbols.erase(to_string(identifier));
    return;
  }

  if (parent) {
    parent->remove(identifier);
  }
}

SP<symbol_t> scope_t::add(const path_t &identifier, SP<type_t> type, bool is_mutable) {
  symbols[to_string(identifier)] = std::make_shared<symbol_t>(symbol_t{
      .name = identifier,
      .type = type,
      .is_mutable = is_mutable
    });
  return symbols.at(to_string(identifier));
}

void scope_t::add_template(SP<binding_decl_t> node) {
  templates.push_back(node);
}

std::vector<SP<binding_decl_t>>
scope_t::candidates(const path_t &path) {
  std::vector<SP<binding_decl_t>> result;
  for (auto &template_ : templates) {
    if (path.segments.size() != template_->name.segments.size()) continue;

    bool match = true;

    for (auto i = 0; i < path.segments.size(); i++) {
      match = match && path.segments[i].name == template_->name.segments[i].name;
      if (path.segments[i].generic_args.size() != template_->name.segments[i].generic_args.size()) match = false;
      if (!match) break;
    }

    if (match)
      result.push_back(template_);
  }

  if (parent) {
    result.insert_range(result.begin(), parent->candidates(path));
  }
  return result;
}
