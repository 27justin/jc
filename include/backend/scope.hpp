#pragma once

#include "backend/type.hpp"
#include "frontend/ast.hpp"
#include "backend/type_registry.hpp"
#include <map>

struct scope_t {
  SP<symbol_t> resolve(const path_t &identifier);
  SP<symbol_t> add(const path_t &identifier, SP<type_t>,
                   bool is_mutable = false);
  void remove(const path_t &identifier);

  void add_template(SP<binding_decl_t>);

  std::vector<SP<binding_decl_t>>
  candidates(const path_t &path);

  scope_t(SP<scope_t>);

  void merge(const scope_t&);

  type_registry_t types;
private:
  std::map<std::string, SP<symbol_t>> symbols;
  std::vector<SP<binding_decl_t>> templates;
  SP<scope_t> parent;
};

