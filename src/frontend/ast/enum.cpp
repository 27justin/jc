#include "frontend/ast.hpp"

enum_variant_node_t::enum_variant_node_t(const std::string                  &name,
                                         std::vector<UP<parameter_node_t>> &&fields)
  : identifier(name)
  , fields(std::move(fields)) {}

enum_variant_node_t::enum_variant_node_t(const std::string &name, UP<ast_node_t> &&explicit_value)
  : identifier(name)
  , explicit_tag_value(std::move(explicit_value)) {}

enum_definition_node_t::enum_definition_node_t(std::vector<UP<enum_variant_node_t>> &&variants)
  : variants(std::move(variants)) {}

#include "frontend/dumper.hpp"

static bool _registered = []() {
  ast_dumper_t::register_node(typeid(enum_definition_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const enum_definition_node_t *>(base);

                                ctx.out << "[Enum\n";
                                ctx.indent++;
                                for (auto &memb : node->variants) {
                                  ctx.print_indent();
                                  ctx.out << ctx.dump(memb.get()) << "\n";
                                }
                                ctx.indent--;
                                ctx.print_indent();
                                ctx.out << "]\n";
                              });

  ast_dumper_t::register_node(typeid(enum_variant_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const enum_variant_node_t *>(base);

                                ctx.out << "[EnumVariant " << node->identifier;
                                if (node->fields.size() > 0) {
                                  ctx.indent++;
                                  for (auto &memb : node->fields) {
                                    ctx.print_indent();
                                    ctx.out << ctx.dump(memb.get()) << "\n";
                                  }
                                  ctx.indent--;
                                  ctx.print_indent();
                                }

                                if (node->explicit_tag_value) {
                                  ctx.out << " = " << ctx.dump(node->explicit_tag_value.get());
                                }
                                ctx.out << "]";
                              });
  return true;
}();
