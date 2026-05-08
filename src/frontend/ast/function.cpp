#include "frontend/ast.hpp"

parameter_node_t::parameter_node_t(std::optional<std::string>     name,
                                   std::unique_ptr<type_node_t> &&type,
                                   bool                           is_mutable)
  : name(name)
  , type(std::move(type))
  , is_mutable(is_mutable) {}

function_node_t::function_node_t(UP<function_type_node_t> &&function_type, UP<block_node_t> &&body)
  : function_type(std::move(function_type))
  , body(std::move(body)) {}

function_call_node_t::function_call_node_t(UP<ast_node_t>              &&callee,
                                           std::vector<UP<ast_node_t>> &&arguments)
  : callee(std::move(callee))
  , arguments(std::move(arguments)) {}

named_argument_node_t::named_argument_node_t(const std::string &name, UP<ast_node_t> &&value)
  : name(name)
  , value(std::move(value)) {}

#include "frontend/dumper.hpp"

static bool _registered = []() {
  ast_dumper_t::register_node(typeid(function_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto *node = static_cast<const function_node_t *>(base);

                                ctx.out << ctx.dump(node->function_type.get()) << "\n";
                                ctx.print_indent();
                                ast_dumper_t::dump(node->body.get(), ctx);
                              });

  ast_dumper_t::register_node(typeid(function_call_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto *node = static_cast<const function_call_node_t *>(base);

                                ctx.out << "[Call " << ctx.dump(node->callee.get());
                                if (node->arguments.size() > 0) {
                                  ctx.out << " with (";
                                  for (auto i = 0; i < node->arguments.size(); ++i) {
                                    ctx.out << ctx.dump(node->arguments[i].get());
                                    if (i < node->arguments.size() - 1)
                                      ctx.out << ", ";
                                  }
                                  ctx.out << ")";
                                }
                                ctx.out << "]";
                              });

  return true;
}();
