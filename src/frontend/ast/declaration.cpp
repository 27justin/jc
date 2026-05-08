#include "frontend/ast.hpp"

declaration_node_t::declaration_node_t(UP<ast_node_t> &&value)
  : value(std::move(value)) {}

void
declaration_node_t::visit(const std::function<void(ast_node_t &)> &visitor) {
  visitor(*this);
  if (where)
    where->visit(visitor);
  if (declared_type)
    declared_type->visit(visitor);
  if (value)
    value->visit(visitor);
}

#include "frontend/dumper.hpp"

static bool _registered = []() {
  ast_dumper_t::register_node(
    typeid(declaration_node_t), [](const ast_node_t *base, dump_context_t &ctx) {
      auto *node = static_cast<const declaration_node_t *>(base);
      ctx.out << "(" << (node->storage_class == declaration_node_t::var ? "var" : "let") << " "
              << ctx.dump(node->where.get());

      if (node->declared_type)
        ctx.out << " as " << ctx.dump(node->declared_type.get());

      ctx.out << "\n";

      ctx.print_indent();
      ctx.indent++;
      ctx.out << ctx.dump(node->value.get());
      ctx.out << ")\n";
      ctx.indent--;
    });
  return true;
}();
