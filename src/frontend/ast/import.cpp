#include "frontend/ast.hpp"

px_import_node_t::px_import_node_t(UP<path_node_t> &&path)
  : path(std::move(path)) {}

file_import_node_t::file_import_node_t(const std::string &path)
  : path(path) {}

c_import_node_t::c_import_node_t(const std::string &identifier, UP<type_node_t> &&type)
  : identifier(identifier)
  , type(std::move(type)) {}

#include "frontend/dumper.hpp"
static bool _registered = []() {
  ast_dumper_t::register_node(typeid(px_import_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const px_import_node_t *>(base);
                                ctx.out << "[LibraryImport " << ctx.dump(node->path.get()) << "]";
                              });

  ast_dumper_t::register_node(typeid(file_import_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const file_import_node_t *>(base);
                                ctx.out << "[FileImport " << node->path << "]";
                              });

  ast_dumper_t::register_node(
    typeid(c_import_node_t), [](const ast_node_t *base, dump_context_t &ctx) {
      auto node = static_cast<const c_import_node_t *>(base);
      ctx.out << "[C Import " << node->identifier << " as " << ctx.dump(node->type.get()) << "]";
    });

  return true;
}();
