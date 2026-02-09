#pragma once

#include "frontend/ast.hpp"

struct template_registry_t {
  std::vector<SP<binding_decl_t>> templates;

  std::vector<SP<binding_decl_t>> candidates(const path_t &path);
};

