#include "frontend/ast.hpp"

function_type_node_t::function_type_node_t(UP<type_node_t>                   &&return_type,
                                           std::vector<UP<parameter_node_t>> &&params)
  : return_type(std::move(return_type))
  , parameters(std::move(params)) {}

#include "frontend/dumper.hpp"

static bool _registered = []() {
  ast_dumper_t::register_node(typeid(function_type_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const function_type_node_t *>(base);

                                ctx.out << "fn (";
                                for (auto i = 0; i < node->parameters.size(); ++i) {
                                  auto &param = node->parameters[i];

                                  if (param->is_mutable)
                                    ctx.out << "var ";
                                  if (param->name) {
                                    ctx.out << *param->name << ": ";
                                  }
                                  ctx.out << ctx.dump(param->type.get());

                                  if (i < node->parameters.size() - 1)
                                    ctx.out << ", ";
                                }

                                ctx.out << ") -> ";
                                if (!node->return_type)
                                  ctx.out << "void";
                                else
                                  ctx.out << ctx.dump(node->return_type.get());
                              });

  ast_dumper_t::register_node(typeid(variable_array_type_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const variable_array_type_node_t *>(base);

                                ctx.out << "[StackArray " << ctx.dump(node->size.get()) << " * "
                                        << ctx.dump(node->element_type.get()) << "]";
                              });

  ast_dumper_t::register_node(
    typeid(const_array_type_node_t), [](const ast_node_t *base, dump_context_t &ctx) {
      auto node = static_cast<const const_array_type_node_t *>(base);
      ctx.out << "[ConstArray " << node->size << " * " << ctx.dump(node->element_type.get()) << "]";
    });

  ast_dumper_t::register_node(typeid(pointer_type_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const pointer_type_node_t *>(base);
                                ctx.out << "[Pointer ";
                                for (auto &nullability : node->indirections) {
                                  ctx.out << (nullability == PT_NULLABLE ? "?" : "!");
                                }
                                ctx.out << ctx.dump(node->element_type.get());
                                ctx.out << "]";
                              });

  ast_dumper_t::register_node(typeid(slice_type_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const slice_type_node_t *>(base);
                                ctx.out << "[Slice of " << ctx.dump(node->element_type.get());
                              });

  ast_dumper_t::register_node(typeid(identifier_type_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const identifier_type_node_t *>(base);
                                ctx.out << "[Type " << ctx.dump(node->path.get()) << "]";
                              });

  ast_dumper_t::register_node(typeid(tuple_type_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const tuple_type_node_t *>(base);
                                ctx.out << "[Tuple ";
                                for (auto i = 0; i < node->members.size(); ++i) {
                                  auto &member = node->members[i];
                                  if (member.is_explicit)
                                    ctx.out << member.name << ": ";
                                  ctx.out << ctx.dump(member.type.get());
                                  if (i < node->members.size() - 1)
                                    ctx.out << ", ";
                                }
                                ctx.out << "]";
                              });

  return true;
}();
