#include "frontend/ast.hpp"

contextual_node_t::contextual_node_t(UP<path_node_t> &&primary)
  : primary(std::move(primary)) {}

contextual_node_t::contextual_node_t()
  : primary(nullptr) {}

#include "frontend/dumper.hpp"

static bool _registered = []() {
  ast_dumper_t::register_node(
    typeid(contextual_node_t), [](const ast_node_t *base, dump_context_t &ctx) {
      auto node = static_cast<const contextual_node_t *>(base);
      ctx.out << "[ContextLookup " << ctx.dump(node->primary.get()) << "]";
    });

  return true;
}();
