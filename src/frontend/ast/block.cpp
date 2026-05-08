#include "frontend/ast.hpp"

block_node_t::block_node_t(std::vector<UP<ast_node_t>> &&statements)
  : statements(std::move(statements)) {}

#include "frontend/dumper.hpp"

static bool _registered = []() {
  ast_dumper_t::register_node(typeid(block_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const block_node_t *>(base);
                                ctx.indent++;
                                for (auto &n : node->statements) {
                                  ctx.out << ctx.dump(n.get());
                                }
                                ctx.indent--;
                              });

  return true;
}();
