#include "frontend/ast.hpp"

result_check_node_t::result_check_node_t(UP<ast_node_t> &&expression,
                                         UP<ast_node_t> &&expected,
                                         UP<ast_node_t> &&fallback)
  : expression(std::move(expression))
  , expected(std::move(expected))
  , fallback(std::move(fallback)) {}

#include "frontend/dumper.hpp"

static bool _registered = []() {
  ast_dumper_t::register_node(
    typeid(result_check_node_t), [](const ast_node_t *base, dump_context_t &ctx) {
      auto node = static_cast<const result_check_node_t *>(base);
      ctx.out << "[ResultCheck " << ctx.dump(node->expression.get()) << "\n";
      ctx.indent++;
      ctx.print_indent();
      ctx.out << ctx.dump(node->expected.get()) << " or " << ctx.dump(node->fallback.get()) << "]";
    });

  return true;
}();
