#include <cassert>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>

#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "frontend/ast.hpp"
#include "frontend/ast/declaration.hpp"
#include "frontend/diagnostic.hpp"
#include "frontend/parser.hpp"
#include "frontend/token.hpp"

#include "backend/types/any.hpp"
#include "backend/types/function.hpp"
#include "backend/types/nominal.hpp"

#include <print>

using QT = qualified_type_t *;
using N  = UP<ast_node_t> &;
using std::make_unique;
using A = analyzer_t;

void
A::register_analyzer(std::type_index node_type, analyze_fn fn) {
  _analyzer_registry()[node_type] = std::move(fn);
}

void
A::annotate(ast_node_t &node, qualified_type_t *type) {
  node.set<qualified_type_t *>(type);
}

void
A::annotate(UP<ast_node_t> &node, qualified_type_t *type) {
  node->set<qualified_type_t *>(type);
}

void
A::set_include_directories(const std::vector<std::string> &dirs) {
  include_directories = dirs;
}

QT
A::analyze(N node) {
  return analyze(node.get());
}

void
A::stub_symbol_declaration(const declaration_node_t &node) {
  qualified_type_t *type = nullptr;

  if (!node.where->is<path_node_t>())
    return;
  auto path = node.where->as<path_node_t>();

  auto value = node.value.get();
  // Full function implementation, we only care for the header
  if (auto function = value->as<function_node_t>()) {
    value = function->function_type.get();
  }

  // Just the type (forward declaration)
  if (value->is<function_type_node_t>()) {
    auto type_node   = value->as<function_type_node_t>();
    auto return_type = registry_.ensure<any_type_t>();

    std::vector<qualified_type_t *> parameters;
    for (auto &param : type_node->parameters) {
      parameters.push_back(registry_.ensure<any_type_t>());
    }

    type = registry_.ensure<function_type_t>(return_type, parameters);
  }

  if (value->is<struct_definition_node_t>()) {
    // When we stub a struct, we stub to any_type.
    //
    // This makes it so we can use the struct before it is fully
    // analyzed, but only when it appears as a pointer.
    type = registry_.ensure<any_type_t>();
  }

  if (value->is<enum_definition_node_t>()) {
    // Same as struct above.
    type = registry_.ensure<any_type_t>();
  }

  if (type) {
    // Create nominal type based on the path.
    registry_.ensure<nominal_type_t>(path->to_string(), type);
  }
}

void
A::analyze_type_declaration(ast_node_t &node) {
  auto declaration = node.as<declaration_node_t>();
  if (!declaration->value->is<struct_definition_node_t>() &&
      !declaration->value->is<enum_definition_node_t>())
    return;

  analyze(&node);
}

void
A::analyze_symbol_declaration(ast_node_t &node) {
  auto declaration = node.as<declaration_node_t>();
  if (!declaration->value->is<function_node_t>() && !declaration->value->is<function_type_node_t>())
    return;

  // We only want to analyze the header, for now
  if (auto header = declaration->value->as<function_node_t>()) {
    auto path = declaration->where->template as<path_node_t>()->to_string();
    scope().add(path, analyze(header->function_type.get()));
  } else {
    analyze(&node);
  }
}

void
A::pass_symbols(const std::vector<UP<ast_node_t>> &nodes) {
  // Phase 1: Stub types as any types and create nominal mappings for them
  for (auto &node : nodes) {
    node->visit(
      [this](auto &node) { stub_symbol_declaration(*node.template as<declaration_node_t>()); },
      [](auto &node) {
        // We only want declarations to register as nominal types.
        return node.template is<declaration_node_t>();
      });
  }

  // Phase 2: Fully analyze structs
  for (auto &node : nodes) {
    node->visit(
      [this](auto &node) { analyze_type_declaration(*node.template as<declaration_node_t>()); },
      [](auto &node) {
        // We only want declarations to register as nominal types.
        return node.template is<declaration_node_t>();
      });
  }

  // Phase 3: Analyze symbols (functions, ..)
  for (auto &node : nodes) {
    node->visit(
      [this](auto &node) { analyze_symbol_declaration(*node.template as<declaration_node_t>()); },
      [](auto &node) {
        // We only want declarations to register as nominal types.
        return node.template is<declaration_node_t>();
      });
  }
}

QT
A::analyze(ast_node_t *node) {
  auto reg = _analyzer_registry();
  auto it  = reg.find(std::type_index(typeid(*node)));

  QT type{};
  if (it != reg.end()) {
    type = it->second(node, *this);
  } else {
    std::cerr << "Missing Analyzer for Node: " << typeid(*node).name() << "\n";
    assert(false && "Internal Compiler Error");
  }
  annotate(*node, type);
  return type;
}

void
analyzer_t::push_scope() {
  scopes_.emplace_back(make_unique<analyzer_scope_t>(*scopes_.back()));
}

void
analyzer_t::pop_scope() {
  scopes_.pop_back();
}

analyzer_scope_t &
analyzer_t::scope() {
  return *scopes_.back();
}

analyzer_scope_t::analyzer_scope_t(analyzer_scope_t &parent)
  : parent(&parent) {}

void
analyzer_scope_t::add(const std::string &name, qualified_type_t *type) {
  symbols[name] = type;
}

qualified_type_t *
analyzer_scope_t::resolve(const std::string &name) {
  if (symbols.contains(name))
    return symbols.at(name);
  if (parent)
    return parent->resolve(name);
  return nullptr;
}

void
A::error(node_location_t loc, diagnostic_t::code_t code, const std::vector<std::string> &args) {
  throw diagnostic_t{
    .level     = diagnostic_level_t::eError,
    .code      = code,
    .arguments = args,
    .source    = loc.source,
    .origin    = loc.location,
  };
}
