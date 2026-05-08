#pragma once

#include "frontend/ast.hpp"
#include <functional>
#include <ostream>

struct dump_context_t;
struct dump_proxy_t {
  const ast_node_t *node;
  dump_context_t   &ctx;

  friend std::ostream &
  operator<<(std::ostream &os, const dump_proxy_t &proxy);
};

struct dump_context_t {
  std::ostream &out;
  ssize_t       indent;

  void
  print_indent();

  dump_proxy_t
  dump(const ast_node_t &);

  dump_proxy_t
  dump(const ast_node_t *);

  dump_proxy_t
  dump(std::unique_ptr<ast_node_t> &);
};

class ast_dumper_t {
  using dump_fn = std::function<void(const ast_node_t *, dump_context_t &)>;

  public:
  static auto &
  registry() {
    static std::unordered_map<std::type_index, dump_fn> registry;
    return registry;
  }

  static void
  dump(const ast_node_t *node, dump_context_t &ctx) {
    if (!node)
      return;
    auto &reg = registry();
    auto  it  = reg.find(std::type_index(typeid(*node)));

    if (it != reg.end()) {
      it->second(node, ctx);
    } else {
      ctx.print_indent();
      ctx.out << "UnknownNode<" << typeid(*node).name() << ">";
    }
    return;
  }

  static void
  register_node(std::type_index type, dump_fn fn) {
    registry()[type] = std::move(fn);
  }
};
