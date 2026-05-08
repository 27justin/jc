#include "frontend/ast.hpp"
#include <sstream>

std::string
path_node_t::to_string() const {
  std::stringstream ss;

  for (auto i = 0; i < segments.size(); ++i) {
    auto &segment = segments[i];
    ss << segment.identifier;

    if (segment.template_args.size() > 0) {
      ss << "<not implemented>";
    }

    if (i < segments.size() - 1)
      ss << ".";
  }

  return ss.str();
}

#include "frontend/dumper.hpp"

static bool _registered = []() {
  ast_dumper_t::register_node(typeid(path_node_t), [](const ast_node_t *base, dump_context_t &ctx) {
    auto *node = static_cast<const path_node_t *>(base);

    for (auto i = 0; i < node->segments.size(); ++i) {
      auto &segment = node->segments[i];
      ctx.out << segment.identifier;

      if (segment.is_templated()) {
        ctx.out << "<";
        for (auto &param : segment.template_args) {
          ctx.out << ctx.dump(param.get());
        }
        ctx.out << ">";
      }

      if (i < node->segments.size() - 1)
        ctx.out << ".";
    }
  });
  return true;
}();
