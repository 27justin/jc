#pragma once

#include "frontend/token.hpp"
#include "frontend/ast.hpp"

#include "frontend/lexer.hpp"
#include "frontend/type.hpp"
#include "frontend/symbol.hpp"
#include "frontend/scope.hpp"
#include "frontend/type_registry.hpp"

#include <map>
#include <string>
#include <memory>

enum class error_kind_t {
  eSymbol,
  eType,
  eScope,
  eSafety,
  eRedeclaration
};

enum class type_error_t {
  eUnknown,
};

struct analyzer_t {
  type_registry_t types;
  lexer_t &lexer;

  std::vector<std::shared_ptr<scope_t>> scopes;
  std::vector<function_type_t *> function_stack;

  analyzer_t(lexer_t &lex) : lexer(lex) {
    scopes.push_back(std::make_shared<scope_t>());
  }

  void analyze(program_node_t &);

  void error(error_kind_t, source_range_t, const std::string &message);
};

