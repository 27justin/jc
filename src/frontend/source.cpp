#include "frontend/source.hpp"
#include "frontend/diagnostic.hpp"

#include <cstring>
#include <string_view>
#include <cassert>

source_t::source_t(std::string_view view, const std::string &name) : filename(name) {
  start = new char[view.size()];
  std::memcpy((void *) start, view.data(), view.size());
  end = start + view.size();
  pointer = start;
};

source_t::source_t(const std::string &str, const std::string &name) : filename(name) {
  start = new char[str.size()];
  std::memcpy((void *) start, str.data(), str.size());
  end = start + str.size();
  pointer = start;
}

source_t::source_t(const char *str, const std::string &name) : filename(name) {
  auto len = std::strlen(str);

  start = new char[len];
  std::memcpy((void *) start, str, len);
  end = start + len;
  pointer = start;
}

source_t::~source_t() {
  delete[] start;
}

bool source_t::eof() const {
  return pointer == end;
}

char source_t::next() {
  if (eof()) throw warn(*this, "End of file reached");
  char c = *pointer++;
  if (c == '\n') {
    column_ = -1;
    line_++;
  } else {
    column_++;
  }
  return c;
}

std::string_view source_t::string(const source_location_t &location) const {
  int64_t start = find_line(location.start.line);
  int64_t end = location.end.line == location.start.line ? start : find_line(location.end.line);

  const char *line_start = this->start + start + location.start.column,
    *line_end = this->start + end + location.end.column;

  assert(line_start > this->start && line_start <= this->end);
  assert(line_end > this->start && line_end <= this->end);
  return std::string_view(line_start, line_end);
}

std::string_view source_t::string(int64_t start, int64_t end) const {
  const char *s = this->start + start;
  const char *e = this->start + end;

  assert(s >= this->start && s <= this->end);
  assert(e >= this->start && e <= this->end);

  return std::string_view(s, e);
}

std::string_view source_t::line(int64_t line) const {
  auto start = find_line(line);
  auto end = find_line(line + 1);

  return std::string_view(this->start + start, this->start + end - 1);
}

int64_t source_t::find_line(int64_t line_no) const {
  const char *p = start;
  int64_t cur_line = 1;
  while (p < end && cur_line < line_no) {
    if (*p++ == '\n') cur_line++;
  }
  return p - start;
}

int64_t source_t::line() const {
  return line_;
}

int64_t source_t::column() const {
  return column_;
}

std::string_view source_t::name() const {
  return filename;
}

void
source_t::push() {
  states.push_back(state_t {
      .column = column_, .line = line_,
      .offset = (end - start)
    });
}

void
source_t::pop() {
  auto state = states.back();

  line_ = state.line;
  column_ = state.column;
  pointer = start + state.offset;

  states.pop_back();
}
