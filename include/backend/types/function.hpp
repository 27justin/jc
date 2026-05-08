#include "backend/type.hpp"

class function_type_t : public qualified_type_t {
  public:
  qualified_type_t               *return_type;
  std::vector<qualified_type_t *> parameters;

  function_type_t(qualified_type_t *, const std::vector<qualified_type_t *> &);

  bool
  equals(const qualified_type_t &) const override;

  ssize_t
  size() const override;

  std::string
  to_string() const override;
};

class annotated_function_type_t : public function_type_t {
  std::vector<std::string> labels;

  public:
  annotated_function_type_t(qualified_type_t *,
                            const std::vector<qualified_type_t *> &,
                            const std::vector<std::string> &);

  bool
  equals(const qualified_type_t &) const override;
  bool
  castable(cast_mode_t, const qualified_type_t &) const override;
  const qualified_type_t *
  underlying_type() const override;
  std::string
  to_string() const override;

  ssize_t
  index_by_label(const std::string &) const;
  std::string
  label_by_index(ssize_t index) const;
};
