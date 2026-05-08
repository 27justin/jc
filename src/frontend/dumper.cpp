#include "frontend/dumper.hpp"

void
dump_context_t::print_indent() {
  for (ssize_t i = 0; i < indent; ++i)
    out << "  ";
}

dump_proxy_t
dump_context_t::dump(const ast_node_t &node) {
  return dump_proxy_t{ &node, *this };
}

dump_proxy_t
dump_context_t::dump(const ast_node_t *node) {
  return dump_proxy_t{ node, *this };
}

dump_proxy_t
dump_context_t::dump(std::unique_ptr<ast_node_t> &node) {
  return dump_proxy_t{ node.get(), *this };
}

std::ostream &
operator<<(std::ostream &os, const dump_proxy_t &proxy) {
  ast_dumper_t::dump(proxy.node, proxy.ctx);
  return os;
}
