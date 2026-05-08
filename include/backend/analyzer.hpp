#pragma once

#include "backend/type.hpp"
#include "backend/type_registry.hpp"
#include "frontend/ast.hpp"
#include "frontend/diagnostic.hpp"
#include "frontend/parser.hpp"
#include "frontend/source.hpp"

#include <unordered_map>
#include <vector>

#define UNREACHABLE assert(false && "Unreachable")

struct analyzer_scope_t {
  std::map<std::string, qualified_type_t *> symbols;
  analyzer_scope_t                         *parent = nullptr;

  analyzer_scope_t() = default;
  explicit analyzer_scope_t(analyzer_scope_t &);
  analyzer_scope_t(analyzer_scope_t &&) = delete;

  void
  operator=(const analyzer_scope_t &) = delete;
  void
  operator=(analyzer_scope_t &&) = delete;

  qualified_type_t *
  resolve(const std::string &name);

  template<typename _Type, typename... _Args>
  void
  add(const std::string &name, _Args &&...args) {
    symbols[name] = std::make_unique<_Type>(std::forward<_Args>(args)...);
  }

  void
  add(const std::string &name, qualified_type_t *);
};

struct analyzer_t {
  using analyze_fn = std::function<qualified_type_t *(ast_node_t *, analyzer_t &)>;
  type_registry_t registry_;

  std::vector<UP<analyzer_scope_t>> scopes_;

  static auto &
  _analyzer_registry() {
    static std::map<std::type_index, analyze_fn> analyzers;
    return analyzers;
  }

  static void register_analyzer(std::type_index, analyze_fn);

  template<typename _NodeType>
  static void
  register_analyzer(std::function<qualified_type_t *(_NodeType &, analyzer_t &)> func) {
    _analyzer_registry()[typeid(_NodeType)] = [func](ast_node_t *node,
                                                     analyzer_t &A) -> qualified_type_t * {
      return func(*static_cast<_NodeType *>(node), A);
    };
  }

  analyzer_t(std::shared_ptr<source_t> src)
    : source(src) {
    scopes_.emplace_back(make_unique<analyzer_scope_t>());
  };

  void
  set_include_directories(const std::vector<std::string> &);

  qualified_type_t *
  analyze(UP<ast_node_t> &);

  qualified_type_t *
  analyze(ast_node_t *);

  // Preliminary symbol pass, registers nominal symbols to be
  // self-referential.
  void
  pass_symbols(const std::vector<UP<ast_node_t>> &);

  void
  stub_symbol_declaration(const declaration_node_t &);

  void
  analyze_type_declaration(ast_node_t &node);

  void
  analyze_symbol_declaration(ast_node_t &node);

  analyzer_scope_t &
  scope();

  void
  push_scope();

  void
  pop_scope();

  qualified_type_t *
  resolve_fully_qualified_path(const std::vector<std::string> &path);

  qualified_type_t *
  resolve_type_path(qualified_type_t *, const std::string &);

  std::vector<qualified_type_t *> type_hints;

  void
  error(node_location_t, diagnostic_t::code_t, const std::vector<std::string> &args);

  using string_list = std::vector<std::string>;
  string_list include_directories;

  bool
  is_lvalue(const ast_node_t *) const;

  private:
  std::shared_ptr<source_t> source;

  // ----------
  //   Analysis
  // ----------

  void
  annotate(ast_node_t &, qualified_type_t *);
  void
  annotate(UP<ast_node_t> &, qualified_type_t *);
};
