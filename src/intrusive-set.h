#pragma once

#include <cassert>
#include <cstddef>
#include <iterator>
#include <type_traits>
#include <utility>

namespace details {
struct set_element_base {
public:
  set_element_base* parent = nullptr;
  set_element_base* left = nullptr;
  set_element_base* right = nullptr;

  void link(set_element_base*& dest, set_element_base* child) {
    dest = child;
    if (child) {
      child->parent = this;
    }
  }

  const set_element_base* min() const {
    auto current = this;
    while (current->left) {
      current = current->left;
    }
    return current;
  }

  set_element_base*& child_ptr() {
    return this == parent->left ? parent->left : parent->right;
  }

  void replace_child(set_element_base* child) {
    child_ptr() = child;
    child->parent = parent;
  }

protected:
  set_element_base() noexcept = default;

  set_element_base(set_element_base&& other) noexcept : left(other.left), right(other.right) {
    if (left) {
      left->parent = this;
      other.left = nullptr;
    }
    if (right) {
      right->parent = this;
      other.right = nullptr;
    }
  }

  set_element_base& operator=(set_element_base&& other) {
    if (left) {
      left->parent = this;
      other.left = nullptr;
    }
    if (right) {
      right->parent = this;
      other.right = nullptr;
    }
    return *this;
  }
};

template <typename Tag>
class set_element : public set_element_base {
public:
  set_element() noexcept = default;
};

template <typename T, typename Key, typename Compare, typename Tag, typename Getter>
class set {
  using node_t = set_element<Tag>;

  static_assert(std::is_base_of_v<node_t, T>, "T must derive from set_element");

private:
  const Key& get_key(const set_element_base* node) const {
    return getter(*static_cast<const T*>(static_cast<const node_t*>(node)));
  }

public:
  struct insert_pos_t {
    insert_pos_t(set_element_base* node) : parent{node} {}

    insert_pos_t(set_element_base* parent, set_element_base** dest) : parent{parent}, dest{dest} {}

    set_element_base* parent;
    set_element_base** dest = nullptr;

    operator bool() const {
      return dest != nullptr;
    }

    set_element_base* node() {
      assert(parent != nullptr);
      return parent;
    }
  };

  node_t& sentinel;
  [[no_unique_address]] Compare compare;
  [[no_unique_address]] Getter getter;

  bool equal(const Key& left, const Key& right) const {
    return !compare(left, right) && !compare(right, left);
  }

  class set_iterator {
  public:
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = T;
    using difference_type = ptrdiff_t;
    using reference = T&;
    using pointer = T*;

  private:
    const set_element_base* current = nullptr;

  public:
    set_iterator(const set_element_base* current) noexcept : current(current) {}

    const set_element_base* base() const {
      return current;
    }

    set_iterator() noexcept = default;

    set_iterator& operator++() noexcept {
      if (current->right) {
        current = current->right->min();
      } else {
        set_element_base* tmp = current->parent;
        while (tmp != nullptr && current == tmp->right) {
          current = tmp;
          tmp = tmp->parent;
        }
        current = tmp;
      }
      return *this;
    }

    set_iterator operator++(int) noexcept {
      set_iterator tmp = *this;
      ++*this;
      return tmp;
    }

    set_iterator& operator--() {
      if (current->left) {
        current = current->left;
        while (current->right) {
          current = current->right;
        }
      } else {
        set_element_base* tmp = current->parent;
        while (tmp != nullptr && current == tmp->left) {
          current = tmp;
          tmp = tmp->parent;
        }
        current = tmp;
      }
      return *this;
    }

    set_iterator operator--(int) noexcept {
      set_iterator tmp = *this;
      --*this;
      return tmp;
    }

    const T& operator*() const noexcept {
      return *static_cast<const T*>(static_cast<const node_t*>(current));
    }

    const T* operator->() const noexcept {
      return &(**this);
    }

    friend bool operator==(const set_iterator& left, const set_iterator& right) {
      return left.current == right.current;
    }
  };

  // O(1)
  explicit set(node_t& sentinel, Compare&& compare = Compare(), Getter&& getter = Getter())
      : sentinel(sentinel),
        compare(std::move(compare)),
        getter(std::move(getter)) {}

  // O(1)
  ~set() = default;

  set(const set&) = delete;
  set& operator=(const set&) = delete;

  using value_type = T;

  using reference = T&;
  using const_reference = const T&;

  using pointer = T*;
  using const_pointer = const T*;

  using iterator = set_iterator;
  using const_iterator = set_iterator;

  // nothrow
  const_iterator begin() const noexcept {
    return const_iterator(sentinel.min());
  }

  // nothrow
  const_iterator end() const noexcept {
    return const_iterator(&sentinel);
  }

  iterator insert(insert_pos_t pos, T& element) {
    assert(pos);
    auto ptr = static_cast<node_t*>(std::addressof(element));
    ptr->parent = pos.parent;
    *pos.dest = ptr;
    return iterator(ptr);
  }

  // O(h) strong
  insert_pos_t insert_pos(const Key& key) {
    set_element_base* tmp = sentinel.left;
    if (tmp == nullptr) {
      return {&sentinel, &sentinel.left};
    }
    while (true) {
      if (compare(key, get_key(tmp))) {
        if (tmp->left) {
          tmp = tmp->left;
        } else {
          return {tmp, &tmp->left};
        }
      } else if (compare(get_key(tmp), key)) {
        if (tmp->right) {
          tmp = tmp->right;
        } else {
          return {tmp, &tmp->right};
        }
      } else {
        return {tmp};
      }
    }
  }

  // O(h) nothrow
  iterator erase(const_iterator pos) noexcept {
    auto current = const_cast<set_element_base*>(pos.base());
    auto next = const_cast<set_element_base*>((++pos).base());
    if (!current->left && !current->right) {
      current->child_ptr() = nullptr;
    } else if (current->left && current->right) {
      next->parent->link(next->child_ptr(), next->right);
      current->replace_child(next);
      next->link(next->left, current->left);
      next->link(next->right, current->right);
    } else {
      set_element_base* child = current->left ? current->left : current->right;
      current->replace_child(child);
    }
    current->left = nullptr;
    current->right = nullptr;
    return iterator(next);
  }

  // O(h) strong
  const_iterator lower_bound(const Key& key) const {
    set_element_base* tmp = sentinel.left;
    while (tmp != nullptr && !equal(key, get_key(tmp))) {
      if (compare(key, get_key(tmp))) {
        if (tmp->left) {
          tmp = tmp->left;
        } else {
          return const_iterator(tmp);
        }
      } else {
        if (tmp->right) {
          tmp = tmp->right;
        } else {
          return ++const_iterator(tmp);
        }
      }
    }
    return tmp ? const_iterator(tmp) : end();
  }

  // O(h) strong
  const_iterator upper_bound(const Key& key) const {
    auto tmp = lower_bound(key);
    if (tmp != end() && equal(get_key(tmp.base()), key)) {
      ++tmp;
    }
    return tmp;
  }

  // O(h) strong
  const_iterator find(const Key& key) const {
    auto tmp = lower_bound(key);
    if (tmp != end() && equal(get_key(tmp.base()), key)) {
      return tmp;
    }
    return end();
  }

  // O(1) nothrow
  friend void swap(set& left, set& right) noexcept {
    std::swap(left.compare, right.compare);
    std::swap(left.getter, right.getter);
    std::swap(left.sentinel.left, right.sentinel.left);
    if (left.sentinel.left) {
      left.sentinel.left->parent = &left.sentinel;
    }
    if (right.sentinel.left) {
      right.sentinel.left->parent = &right.sentinel;
    }
  }
};
} // namespace details
