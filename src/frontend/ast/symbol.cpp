#include "frontend/ast.hpp"

symbol_node_t::symbol_node_t(std::unique_ptr<path_node_t> &&path)
  : path(std::move(path)) {}

#include "frontend/dumper.hpp"
static bool _registered = []() {
  ast_dumper_t::register_node(typeid(symbol_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const symbol_node_t *>(base);
                                ctx.out << ctx.dump(node->path.get());
                              });

  return true;
}();
