#include "frontend/ast.hpp"

tuple_member_node_t::tuple_member_node_t(const std::string &name, UP<ast_node_t> &&initializer)
  : name(name)
  , value(std::move(initializer)) {}

tuple_member_node_t::tuple_member_node_t(UP<ast_node_t> &&initializer)
  : value(std::move(initializer)) {}

tuple_value_node_t::tuple_value_node_t(std::vector<UP<tuple_member_node_t>> &&values)
  : values(std::move(values)) {}

#include "frontend/dumper.hpp"

static bool _registered = []() {
  ast_dumper_t::register_node(typeid(tuple_value_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const tuple_value_node_t *>(base);
                                ctx.out << "[Tuple \n";

                                ctx.indent++;
                                for (auto &memb : node->values) {
                                  ctx.print_indent();
                                  ctx.out << ctx.dump(memb.get()) << "\n";
                                }
                                ctx.indent--;
                                ctx.print_indent();
                                ctx.out << "]";
                              });

  ast_dumper_t::register_node(typeid(tuple_member_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const tuple_member_node_t *>(base);
                                ctx.out << "[TupleMember ";
                                if (node->name)
                                  ctx.out << *node->name << " ";
                                ctx.out << ctx.dump(node->value.get()) << "]";
                              });
  return true;
}();
