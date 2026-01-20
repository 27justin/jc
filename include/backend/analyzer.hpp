#pragma once

#include "frontend/ast.hpp"
#include "frontend/diagnostic.hpp"
#include "frontend/parser.hpp"

#include "frontend/source.hpp"
#include "type.hpp"
#include "type_registry.hpp"
#include "frontend/template_registry.hpp"
#include "symbol.hpp"
#include "scope.hpp"

#include <vector>
#include <map>

struct semantic_info_t {
  translation_unit_t unit;
  std::map<SP<ast_node_t>, SP<type_t>> resolved_types;
};

struct analyze_error_t {
  diagnostic_stack_t diagnostics;
};

struct call_frame_t {
  std::vector<SP<type_t>> expected_params;
  std::vector<SP<ast_node_t>> effective_args;
  SP<type_t> return_type;
  bool is_var_args;
};

struct analyzer_t {
  semantic_info_t
  analyze(translation_unit_t tu);

  analyzer_t(source_t &src) : source(src) {};
private:
  using string_list = std::vector<std::string>;

  static constexpr const char INVALID_RETURN_STMT[] =
      "Invalid return statement";
  static constexpr const char INVALID_RETURN_STMT_DETAIL[] = "Return statement without a function frame, returning here is illegal.";

  static constexpr const char ILLEGAL_TYPE[] = "Unexpected type";
  static constexpr const char ILLEGAL_TYPE_DETAIL[] = "Unexpected type `{}`, expected `{}`";

  static constexpr const char UNKNOWN_SYMBOL[] = "Unknown symbol";
  static constexpr const char UNKNOWN_SYMBOL_DETAIL[] = "Symbol `{}` was not found in the current scope";

  static constexpr const char ILLEGAL_OPERATION[] = "Illegal operation";
  static constexpr const char ILLEGAL_OPERATION_DETAIL[] = "Operation `{}` is not allowed on expressions of type `{}`, and `{}`";

  static constexpr const char NOT_A_FUNCTION[] = "Illegal operation";
  static constexpr const char NOT_A_FUNCTION_DETAIL[] = "Tried to invoke `{}`, but symbol is of type `{}`";

  static constexpr const char ARG_COUNT_MISMATCH[] = "Too {} arguments";
  static constexpr const char ARG_COUNT_MISMATCH_DETAIL[] = "Function `{}` expected {} arguments, got {}";

  static constexpr const char MUTABILITY_VIOLATION[] = "Mutability violation";
  static constexpr const char MUTABILITY_VIOLATION_DETAIL[] = "Tried to assign to symbol `{}`, but symbol is constant.";
  static constexpr const char MUTABILITY_VIOLATION_RECOMMEND[] =
      "Consider changing the binding to `var`, or remove the violation.";

  static constexpr const char UNKNOWN_TYPE[] = "Unknown type";
  static constexpr const char UNKNOWN_TYPE_DETAIL[] = "Type `{}` is not known at this point.";

  static constexpr const char INFER_DETAIL[] =
      "Could not infer value of this expression.";
  static constexpr const char INFER_RECOMMEND[] = "Specify the type of the binding using `{}: <type>`.";

  static constexpr const char INVALID_TYPE_ASSIGNMENT[] =
      "Invalid type";
  static constexpr const char INVALID_TYPE_ASSIGNMENT_DETAIL[] = "Type `{}` has no physical size, therefore can not be used here.";

  static constexpr const char INVALID_ASSIGNMENT[] =
      "Invalid assignment";
  static constexpr const char INVALID_ASSIGNMENT_DETAIL[] = "Tried to assign expression of type `{}` to an lvalue of type `{}`";

  static constexpr const char UNKNOWN_MEMBER[] =
      "Unknown member";
  static constexpr const char UNKNOWN_MEMBER_DETAIL[] = "Member `{}` is not known on type `{}`";

  static constexpr const char INVALID_DEREF[] =
      "Invalid dereference";
  static constexpr const char INVALID_DEREF_DETAIL[] = "Type `{}` is not a pointer, and thus not dereferenceable.";

  static constexpr const char INVALID_CAST[] =
      "Invalid cast";
  static constexpr const char INVALID_CAST_DETAIL[] = "Type `{}` cannot be cast into type `{}`.";

  source_t &source;
  std::map<SP<ast_node_t>, SP<type_t>> resolved_types;

  type_registry_t types;
  template_registry_t templates;

  std::vector<SP<scope_t>> scope_stack;
  std::vector<SP<type_t>> function_stack;
  std::vector<SP<type_t>> type_infer_stack; //< Used to infer types in certain cases.

  // Push a function to the stack
  void push_function_frame(SP<type_t>);

  // Pop a function from the stack
  void pop_function_frame();

  // Return the most recent function frame
  SP<type_t> get_function_frame();
  bool has_function_frame();

  // --------------------------------------

  // Push a scope to the stack
  SP<scope_t> push_scope();

  // Pop a scope from the stack
  void pop_scope();

  // Return the most recent scope from the stack
  SP<scope_t> get_scope();


  // --------------------------------------

  void push_type_infer(SP<type_t>);
  void pop_type_infer();
  SP<type_t> get_type_infer();
  bool has_type_infer();

  // --------------------------------------


  // Return a type that is the pointer to A
  SP<type_t> get_pointer_to(SP<type_t> A);

  // Return a type that is a function pointer to a function returning
  // R with argument types `args`
  SP<type_t> get_function_pointer(SP<type_t> R, std::vector<SP<type_t>> args);

  diagnostic_stack_t diagnostics;

  // ----------
  //   Analysis
  // ----------
  using QT = SP<type_t>;
  using N = SP<ast_node_t>;

  QT resolve_primitive_binop(binop_type_t op, QT L, QT R);

  bool is_lvalue(SP<ast_node_t>);
  // Returns true, if a symbol is mutable
  bool is_mutable(SP<ast_node_t> A);

  // Returns true, if type A is trivially coercible into type B
  //
  // Example:
  // - is_coercible(i32, i64) -> true, can't lose information
  // - is_coercible(i32, u64) -> false, might lose sign
  // - is_coercible(i64, i32) -> false, might lose information
  // - is_coercible(!u8, ?u8) -> true, a non-nullable pointer can be coerced
  // into nullable
  // - is_coercible(?u8, !u8) -> false, a nullable pointer can't be coerced into
  // a non-nullable one
  bool is_coercible(SP<type_t> A, SP<type_t> B);

  // Returns true, if type A is explicitly castable into type B
  //
  // Returns true on any case `is_coercible` would return true, and
  // additional type checks for opaque types.
  bool is_castable(SP<type_t> A, SP<type_t> B);

  /// Function to desugar one expression into another.
  template <typename Data>
  void desugar(SP<ast_node_t> node, ast_node_t::kind_t new_kind, Data &&data) {
    node->reset();
    node->kind = new_kind;
    node->as.raw = new Data(std::move(data));
  }

  void register_template(N);

  QT analyze_type(N);
  QT analyze_expression(N);
  QT analyze_block(N);
  QT analyze_node(N);
  QT analyze_literal(N);
  QT analyze_lookup(N);
  QT analyze_binop(N);

  call_frame_t prepare_call_frame(call_expr_t *);
  QT analyze_call(N);

  QT analyze_declaration(N);

  QT analyze_function_parameter(N);
  QT analyze_function_decl(N);
  QT analyze_function(N);

  QT analyze_return(N);
  QT analyze_symbol(N);
  QT analyze_extern(N);
  QT analyze_struct(N);
  QT analyze_member_access(N);

  QT analyze_addr_of(N);
  QT analyze_unary(N);
  QT analyze_self(N);

  QT analyze_if(N);
  QT analyze_for(N);
  QT analyze_while(N);
  QT analyze_type_alias(N);
  QT analyze_cast(N);
  QT analyze_deref(N);
  QT analyze_nil(N);

  QT analyze_assignment(N);
  QT analyze_attribute(N);

  std::string join(const std::vector<std::string> &, const std::string &separator);
  std::vector<std::string> split(const std::string &, const std::string &);
  SP<symbol_t> resolve_path(const std::vector<std::string> &path);
  void flatten_member_access(N, std::vector<std::string> &path);

  /// @brief Lower an expression into a more usable form Used to
  /// e.g. lower the `.` shorthand, or the member function call into a
  /// fully specified one.
  SP<ast_node_t> lower(SP<ast_node_t> from, SP<ast_node_t> to);

  SP<ast_node_t> coerce(SP<ast_node_t>, SP<type_t>);

  // --------
  //   Errors
  // --------

  void invalid_operation(SP<ast_node_t>, SP<type_t>, SP<type_t>, binop_type_t);
  void unknown_type_error(SP<ast_node_t>);
  void type_error(SP<type_t> expected, SP<type_t> got, SP<ast_node_t> where);
  void var_error(SP<ast_node_t>);
  void infer_error(SP<ast_node_t>);
  void invalid_type_error(SP<type_t>, SP<ast_node_t>);
  void invalid_assignment(SP<type_t>, SP<ast_node_t>);
  void generic_error(SP<ast_node_t>, std::string, std::string, std::string = "");

  void unknown_symbol(SP<ast_node_t>);
  void unknown_member(const std::string &, SP<type_t>, SP<ast_node_t>);

  void invalid_deref(SP<type_t>, SP<ast_node_t>);
  void invalid_cast(SP<type_t>, SP<type_t>, SP<ast_node_t>);
};
