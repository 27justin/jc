#include "frontend/ast.hpp"

string_node_t::string_node_t(const std::string &value)
  : value(value) {}

#include "frontend/dumper.hpp"

static bool _registered = []() {
  ast_dumper_t::register_node(typeid(string_node_t),
                              [](const ast_node_t *base, dump_context_t &ctx) {
                                auto node = static_cast<const string_node_t *>(base);
                                ctx.out << "[String \"" << node->value << "\"]";
                              });
  return true;
}();
