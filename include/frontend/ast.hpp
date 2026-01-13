#pragma once

#include <vector>
#include <string>
#include <memory>
#include <optional>

#include "frontend/token.hpp"
#include "frontend/symbol.hpp"
#include "frontend/scope.hpp"

struct node_t {
  source_range_t location;
  virtual ~node_t() = default;
};

// Represents a literal value (e.g., 5, "hello")
struct literal_expr_t : public node_t {};

struct struct_decl_t;

struct program_node_t : public node_t {
  std::vector<std::unique_ptr<node_t>> declarations;
  std::vector<std::unique_ptr<struct_decl_t>> structs;
  std::vector<std::unique_ptr<node_t>> externs;
};

struct string_literal_t : public literal_expr_t {
  std::string value;
};

struct integer_literal_t : public literal_expr_t {
  std::string value;
};

struct bool_literal_t : public literal_expr_t {
  bool value;
};

struct float_literal_t : public literal_expr_t {
  std::string value;
};

struct identifier_expr_t : public node_t {
  std::string name;
  identifier_expr_t(std::string name) : name(name) {}
  identifier_expr_t() {}
};

struct member_access_expr_t : public node_t {
  std::unique_ptr<node_t> object;
  std::string member_name;
};

// Represents a function call: printf("hi")
struct call_expr_t : public node_t {
  std::unique_ptr<node_t> callee;
  std::vector<std::unique_ptr<node_t>> arguments;
};

// Represents a return statement: return 0;
struct return_stmt_t : public node_t {
    std::unique_ptr<node_t> expression;

    return_stmt_t(std::unique_ptr<node_t> expr) : expression(std::move(expr)) {}
};

struct block_t : public node_t {
  std::vector<std::unique_ptr<node_t>> statements;

  std::shared_ptr<scope_t> scope;
};

struct type_stmt_t : public node_t {
  std::string name;
  std::vector<std::string> refinements;
  bool is_pointer = false,
    is_nullable = false,
    is_array;
  size_t array_len = 0;

  type_stmt_t() = default;
  type_stmt_t(std::string type) : name(type) {}
};

struct var_decl_t : public node_t {
  std::unique_ptr<type_stmt_t> type; // "i32", "f64", or "auto"
  std::string identifier;
  std::optional<std::unique_ptr<node_t>> initial_value;
};

struct struct_decl_t : public node_t {
  std::string name;
  std::vector<std::unique_ptr<var_decl_t>> members;

  std::unique_ptr<struct_type_t> layout;
};

struct binary_expr_t : public node_t {
  token_type_t op;
  std::unique_ptr<node_t> left;
  std::unique_ptr<node_t> right;
};

struct unary_expr_t : public node_t {
  token_type_t op;
  std::unique_ptr<node_t> target;

  unary_expr_t(token_type_t op, std::unique_ptr<node_t> t)
    : op(op), target(std::move(t)) {}
};

struct function_header_t : public node_t {
  std::string name;
  std::unique_ptr<type_stmt_t> return_type;
  std::vector<std::unique_ptr<var_decl_t>> arguments;
};

// Represents the function itself
struct function_decl_t : public node_t {
  std::unique_ptr<function_header_t> header;
  std::unique_ptr<block_t> body; // List of statements
};

struct extern_decl_t : public node_t {
  std::string language;
  std::unique_ptr<node_t> symbol;
};

void dump_ast(const node_t& node, int indent = 0);

