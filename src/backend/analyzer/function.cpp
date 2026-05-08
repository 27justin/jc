#include "backend/types/function.hpp"
#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include <cassert>

#include "backend/types/enum.hpp"
#include "frontend/ast.hpp"
#include "frontend/ast/function.hpp"
#include "frontend/diagnostic.hpp"

static auto _registered = []() {
  analyzer_t::register_analyzer<function_type_node_t>(
    [](function_type_node_t &node, analyzer_t &A) -> qualified_type_t * {
      auto return_type = A.registry_.resolve("void");
      if (node.return_type)
        return_type = A.analyze(node.return_type.get());

      std::vector<qualified_type_t *> parameters;
      std::vector<std::string>        labels;
      for (auto &param : node.parameters) {
        auto param_type = A.analyze(param.get());

        parameters.emplace_back(param_type);
        labels.emplace_back(param->name.value_or(""));
      }

      // Ensure that the base class is also registered
      A.registry_.ensure<function_type_t>(return_type, parameters);
      // ... but do return the annotated version of it.
      //
      // TODO: We need to overwrite, because an empty
      // annotated_function_type_t yields the exact same string
      // representation as an non-annotated ones, in those cases, we
      // need to prefer `annotated_function_type_t`, since it inherits
      // from `function_type_t` anyway.
      return A.registry_.ensure_or_overwrite<annotated_function_type_t>(
        return_type, parameters, labels);
    });

  analyzer_t::register_analyzer<function_node_t>(
    [](function_node_t &node, analyzer_t &A) -> qualified_type_t * {
      auto type    = A.analyze(node.function_type.get());
      auto fn_type = type->as<annotated_function_type_t>();

      A.push_scope();

      std::vector<std::string> labels;
      for (auto i = 0; i < fn_type->parameters.size(); i++) {
        auto param = fn_type->parameters[i];
        auto name  = fn_type->label_by_index(i);

        // Add the parameter to the scope
        if (name.size() > 0)
          A.scope().add(name, param);
      }

      // Analyze the body and get the actual return type.
      auto deduced_return_type = A.analyze(node.body.get());

      if (deduced_return_type->equals(*fn_type->return_type) == false &&
          !deduced_return_type->castable(cast_mode_t::eImplicit, *fn_type->return_type)) {
        A.error(node.function_type->return_type->get<node_location_t>(),
                deduced_return_type->castable(cast_mode_t::eExplicit, *fn_type->return_type)
                  ? diagnostic_t::code_t::incompatible_implicit_cast
                  : diagnostic_t::code_t::incompatible_explicit_cast,
                { deduced_return_type->to_string(), fn_type->return_type->to_string() });
        UNREACHABLE;
      }

      A.pop_scope();

      // Return analyzed function header type.
      return fn_type;
    });

  analyzer_t::register_analyzer<parameter_node_t>(
    [](parameter_node_t &node, analyzer_t &A) -> qualified_type_t * {
      return A.analyze(node.type.get());
    });

  // Function call
  analyzer_t::register_analyzer<function_call_node_t>([](function_call_node_t &node,
                                                         analyzer_t &A) -> qualified_type_t * {
    auto callee = A.analyze(node.callee.get());

    if (!callee || !(callee->is<function_type_t>() || callee->is<variant_constructor_type_t>())) {
      A.error(node.callee->get<node_location_t>(),
              diagnostic_t::code_t::not_a_function,
              { callee ? callee->to_string() : "nil" });
      UNREACHABLE;
      return nullptr;
    }

    if (auto func = callee->as<function_type_t>()) {
      // Check that the argument count is fine
      if (node.arguments.size() != func->parameters.size()) {
        A.error(node.get<node_location_t>(),
                node.arguments.size() < func->parameters.size()
                  ? diagnostic_t::code_t::too_few_arguments
                  : diagnostic_t::code_t::too_few_arguments,
                { std::to_string(func->parameters.size()), std::to_string(node.arguments.size()) });
        UNREACHABLE;
        return nullptr;
      }

      auto annotated = callee->as<annotated_function_type_t>();

      // Validate each argument
      std::vector<bool> param_filled(func->parameters.size(), false);
      for (auto i = 0; i < node.arguments.size(); ++i) {
        auto             &arg           = node.arguments[i];
        size_t            target_slot   = 0;
        qualified_type_t *expected_type = nullptr;

        if (auto named = arg->as<named_argument_node_t>(); named && annotated) {
          auto idx = annotated->index_by_label(named->name);
          if (idx < 0) {
            A.error(arg->get<node_location_t>(),
                    diagnostic_t::code_t::unknown_parameter,
                    { named->name });
            UNREACHABLE;
            return nullptr;
          }
          target_slot = idx;
        } else {
          target_slot = i;
        }

        if (param_filled[target_slot]) {
          A.error(
            arg->get<node_location_t>(),
            diagnostic_t::code_t::parameter_already_filled,
            { annotated ? annotated->label_by_index(target_slot) : std::to_string(target_slot) });
          UNREACHABLE;
          return nullptr;
        }

        expected_type = func->parameters[target_slot];

        // Push a type hint so contextual lookup works
        A.type_hints.push_back(expected_type);

        auto actual_type = A.analyze(arg.get());

        A.type_hints.pop_back();

        if (!actual_type->castable(cast_mode_t::eImplicit, *expected_type)) {
          A.error(
            arg->get<node_location_t>(),
            diagnostic_t::code_t::argument_type_mismatch,
            { annotated ? annotated->label_by_index(target_slot) : std::to_string(target_slot),
              expected_type->to_string(),
              actual_type->to_string() });
          UNREACHABLE;
          return nullptr;
        }
        param_filled[target_slot] = true;
      }

      // Return the functions return type
      return func->return_type;
    }

    if (auto variant_constructor = callee->as<variant_constructor_type_t>()) {
      // Calling a variant constructor decays into the enum itself.
      return variant_constructor->aggregate();
    }

    UNREACHABLE;
  });

  // Named argument, just forward the value.
  analyzer_t::register_analyzer<named_argument_node_t>(
    [](named_argument_node_t &node, analyzer_t &A) -> qualified_type_t * {
      return A.analyze(node.value.get());
    });

  return true;
}();
