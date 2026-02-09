#pragma once

#include "frontend/ast.hpp"
#include "frontend/diagnostic.hpp"
#include "frontend/parser.hpp"

#include "frontend/path.hpp"
#include "frontend/source.hpp"
#include "type.hpp"
#include "symbol.hpp"
#include "scope.hpp"

#include <vector>
#include <map>

struct semantic_info_t {
  translation_unit_t unit;
  SP<scope_t> scope;
};

struct analyze_error_t {
  diagnostic_stack_t diagnostics;
};

enum class value_category_t {
  eLValue, eRValue
};

struct analyzer_t {
  semantic_info_t
  analyze(translation_unit_t tu);

  analyzer_t(std::shared_ptr<source_t> src) : source(src) {};
private:
  using string_list = std::vector<std::string>;

  scope_t &push_scope();
  scope_t &scope();
  void pop_scope();

  std::shared_ptr<source_t> source;

  std::vector<SP<scope_t>> scope_stack;
  std::vector<SP<type_t>> function_stack;
  std::vector<SP<type_t>> type_infer_stack; //< Used to infer types in certain cases.

  diagnostic_stack_t diagnostics;

  // ----------
  //   Analysis
  // ----------
  using QT = SP<type_t>;
  using N = SP<ast_node_t>;

  std::optional<specialized_path_t> current_binding;

  bool is_rvalue(N);
  bool is_lvalue(N);
  bool is_mutable(N);

  QT analyze_node(N);
  QT analyze_binding(N);

  QT analyze_function_decl(N, SP<type_t>);
  QT analyze_function_decl(const function_decl_t&, SP<type_t>);
  QT analyze_function_impl(N, SP<type_t>);
  QT analyze_block(N);
  QT analyze_literal(N);
  QT analyze_symbol(N);
  QT analyze_deref(N);
  QT analyze_type_alias(N);
  QT analyze_struct_decl(N);
  QT analyze_struct_expr(N);
  QT analyze_contract(N);
  QT analyze_declaration(N);
  QT analyze_call(N);
  QT analyze_cast(N);
  QT analyze_binop(N);
  QT analyze_addr_of(N);
  QT analyze_defer(N);
  QT analyze_move(N);
  QT analyze_nil(N);
  QT analyze_if(N);
  QT analyze_assignment(N);

  bool is_static_dispatch(N);
  bool is_dynamic_dispatch(N);

  bool is_cast_convertible(QT from, QT into);
  bool is_implicit_convertible(QT from, QT into);
  bool satisfies_contract(QT type, QT contract);

  void import_source_file(const std::string &);

  QT resolve_receiver(std::optional<specialized_path_t>);
  QT resolve_member_access(QT left, const std::string &member_name);

  QT resolve_binop_result_type(binop_type_t, QT left, QT right);

  value_category_t resolve_value_category(N node);

  QT monomorphize(SP<template_decl_t> template_, specialized_path_t instantiation);

  /// Resolve a type via the type registry, potentially monomorphing templates if type is templated.
  QT resolve_type(const type_decl_t &);
  QT resolve_type(const specialized_path_t &);
  QT resolve_type(const std::string &);

  specialized_path_t resolve_path(const specialized_path_t&);
};
