#pragma once

#include "intrusive-set.h"

#include <concepts>
#include <cstddef>
#include <functional>
#include <memory>
#include <stdexcept>

template <typename Left, typename Right, typename CompareLeft = std::less<Left>,
          typename CompareRight = std::less<Right>>
class bimap {
public:
  using left_t = Left;
  using right_t = Right;

private:
  struct left_tag;
  struct right_tag;

  using set_base_node = details::set_element_base;
  template <typename Tag>
  using set_node = details::set_element<Tag>;
  using left_node = set_node<left_tag>;
  using right_node = set_node<right_tag>;

  struct base_node : left_node, right_node {
    base_node() noexcept = default;
    base_node(base_node&& other) noexcept = default;

    base_node& operator=(base_node&& other) noexcept {
      static_cast<left_node&>(*this) = std::move(other);
      static_cast<right_node&>(*this) = std::move(other);
      return *this;
    }

    ~base_node() = default;
  };

  struct bimap_node : base_node {
    left_t left;
    right_t right;

    template <typename L, typename R>
    bimap_node(L&& left, R&& right) : left(std::forward<L>(left)),
                                      right(std::forward<R>(right)) {}
  };

  struct left_getter_t {
    const left_t& operator()(const bimap_node& node) const {
      return node.left;
    }
  };

  template <typename Tag>
  static const bimap_node* from_tag(const details::set_element_base* node) {
    return static_cast<const bimap_node*>(static_cast<const set_node<Tag>*>(node));
  }

  template <typename Tag>
  static bimap_node* from_tag(details::set_element_base* node) {
    return static_cast<bimap_node*>(static_cast<set_node<Tag>*>(node));
  }

  struct right_getter_t {
    const right_t& operator()(const bimap_node& node) const {
      return node.right;
    }
  };

  size_t sz = 0;

  using left_set_t = details::set<bimap_node, Left, CompareLeft, left_tag, left_getter_t>;
  using right_set_t = details::set<bimap_node, Right, CompareRight, right_tag, right_getter_t>;

  base_node sentinel;
  left_set_t left_set;
  right_set_t right_set;

public:
  using node_t = bimap_node;

private:
  template <typename Base, typename Tag, typename Getter, typename T, typename Pair, typename PairTag,
            typename PairGetter, typename PairT>
  class iterator : Base {
    template <typename, typename, typename, typename>
    friend class bimap;

  private:
    using Base::Base;

    iterator(Base it) : Base(it) {}

  public:
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = T;
    using difference_type = ptrdiff_t;
    using reference = const T&;
    using pointer = const T*;

    iterator() noexcept = default;

    // Элемент на который сейчас ссылается итератор.
    // Разыменование итератора end_left() не определено.
    // Разыменование невалидного итератора не определено.
    const T& operator*() const {
      return Getter()(*from_tag<Tag>(this->base()));
    }

    const T* operator->() const {
      return std::addressof(**this);
    }

    // Переход к следующему по величине left'у.
    // Инкремент итератора end_left() не определен.
    // Инкремент невалидного итератора не определен.
    iterator& operator++() {
      Base::operator++();
      return *this;
    }

    iterator operator++(int) {
      return static_cast<Base&>(*this)++;
    }

    // Переход к предыдущему по величине left'у.
    // Декремент итератора begin_left() не определен.
    // Декремент невалидного итератора не определен.
    iterator& operator--() {
      Base::operator--();
      return *this;
    }

    iterator operator--(int) {
      return static_cast<Base&>(*this)--;
    }

    // left_iterator ссылается на левый элемент некоторой пары.
    // Эта функция возвращает итератор на правый элемент той же пары.
    // end_left().flip() возращает end_right().
    // end_right().flip() возвращает end_left().
    // flip() невалидного итератора не определен.
    iterator<Pair, PairTag, PairGetter, PairT, Base, Tag, Getter, T> flip() {
      return {static_cast<const set_node<PairTag>*>(
          static_cast<const base_node*>(static_cast<const set_node<Tag>*>(this->base())))};
    }

    friend bool operator==(const iterator& left, const iterator& right) {
      return static_cast<const Base&>(left) == static_cast<const Base&>(right);
    }
  };

  bimap(base_node&& sentinel, CompareLeft&& compare_left, CompareRight&& compare_right)
      : sentinel(std::move(sentinel)),
        left_set(sentinel, std::move(compare_left)),
        right_set(sentinel, std::move(compare_right)) {}

  bool left_equal(const left_t& lhs, const left_t& rhs) const {
    return !left_set.compare(lhs, rhs) && !left_set.compare(rhs, lhs);
  }

  bool right_equal(const right_t& lhs, const right_t& rhs) const {
    return !right_set.compare(lhs, rhs) && !right_set.compare(rhs, lhs);
  }

  bool equal(const bimap_node& lhs, const bimap_node& rhs) const {
    return left_equal(lhs.left, rhs.left) && right_equal(lhs.right, rhs.right);
  }

public:
  using left_iterator = iterator<typename left_set_t::iterator, left_tag, left_getter_t, left_t,
                                 typename right_set_t::iterator, right_tag, right_getter_t, right_t>;
  using right_iterator = iterator<typename right_set_t::iterator, right_tag, right_getter_t, right_t,
                                  typename left_set_t::iterator, left_tag, left_getter_t, left_t>;

  // Создает bimap, не содержащий ни одной пары.
  bimap(CompareLeft compare_left = CompareLeft(), CompareRight compare_right = CompareRight())
      : left_set(sentinel, std::move(compare_left)),
        right_set(sentinel, std::move(compare_right)) {}

  // Конструкторы от других и присваивания
  bimap(const bimap& other) : bimap(other.left_set.compare, other.right_set.compare) {
    for (auto it = other.begin_left(); it != other.end_left(); ++it) {
      insert(*it, *it.flip());
    }
  }

  bimap(bimap&& other) noexcept
      : sz(other.sz),
        sentinel(std::move(other.sentinel)),
        left_set(sentinel, std::move(other.left_set.compare)),
        right_set(sentinel, std::move(other.right_set.compare)) {
    other.sz = 0;
  }

  bimap& operator=(const bimap& other) {
    if (this == &other) {
      return *this;
    }
    bimap tmp{other};
    swap(*this, tmp);
    return *this;
  }

  bimap& operator=(bimap&& other) noexcept {
    if (this == &other) {
      return *this;
    }
    bimap tmp{std::move(other)};
    swap(*this, tmp);
    return *this;
  }

  // Деструктор. Вызывается при удалении объектов bimap.
  // Инвалидирует все итераторы ссылающиеся на элементы этого bimap
  // (включая итераторы ссылающиеся на элементы следующие за последними).
  ~bimap() {
    erase_left(begin_left(), end_left());
  }

  friend void swap(bimap& lhs, bimap& rhs) {
    swap(lhs.left_set, rhs.left_set);
    swap(lhs.right_set, rhs.right_set);
    std::swap(lhs.sz, rhs.sz);
  }

  // Вставка пары (left, right), возвращает итератор на left.
  // Если такой left или такой right уже присутствуют в bimap, вставка не
  // производится и возвращается end_left().
  left_iterator insert(const left_t& left, const right_t& right) {
    return perform_insert(left, right);
  }

  left_iterator insert(const left_t& left, right_t&& right) {
    return perform_insert(left, std::move(right));
  }

  left_iterator insert(left_t&& left, const right_t& right) {
    return perform_insert(std::move(left), right);
  }

  left_iterator insert(left_t&& left, right_t&& right) {
    return perform_insert(std::move(left), std::move(right));
  }

  // Удаляет элемент и соответствующий ему парный.
  // erase невалидного итератора не определен.
  // erase(end_left()) и erase(end_right()) не определены.
  // Пусть it ссылается на некоторый элемент e.
  // erase инвалидирует все итераторы ссылающиеся на e и на элемент парный к e.
  left_iterator erase_left(left_iterator it) {
    --sz;
    auto base = it.base();
    right_set.erase(it.flip());
    left_iterator next = left_set.erase(it);
    delete const_cast<node_t*>(from_tag<left_tag>(base));
    return next;
  }

  // Аналогично erase, но по ключу, удаляет элемент если он присутствует, иначе
  // не делает ничего Возвращает была ли пара удалена
  bool erase_left(const left_t& left) {
    left_iterator it = left_set.find(left);
    if (it == end_left()) {
      return false;
    }
    erase_left(it);
    return true;
  }

  right_iterator erase_right(right_iterator it) {
    --sz;
    auto base = it.base();
    left_set.erase(it.flip());
    right_iterator next = right_set.erase(it);
    delete const_cast<node_t*>(from_tag<right_tag>(base));
    return next;
  }

  bool erase_right(const right_t& right) {
    right_iterator it = right_set.find(right);
    if (it == end_right()) {
      return false;
    }
    erase_right(it);
    return true;
  }

  // erase от ренжа, удаляет [first, last), возвращает итератор на последний
  // элемент за удаленной последовательностью
  left_iterator erase_left(left_iterator first, left_iterator last) {
    auto it = first;
    while (it != last) {
      it = erase_left(it);
    }
    return it;
  }

  right_iterator erase_right(right_iterator first, right_iterator last) {
    auto it = first;
    while (it != last) {
      it = erase_right(it);
    }
    return it;
  }

  // Возвращает итератор по элементу. Если не найден - соответствующий end()
  left_iterator find_left(const left_t& left) const {
    return left_set.find(left);
  }

  right_iterator find_right(const right_t& right) const {
    return right_set.find(right);
  }

  // Возвращает противоположный элемент по элементу
  // Если элемента не существует -- бросает std::out_of_range
  const right_t& at_left(const left_t& key) const {
    auto it = find_left(key);
    if (it == end_left()) {
      throw std::out_of_range("No such element");
    }
    return *it.flip();
  }

  const left_t& at_right(const right_t& key) const {
    auto it = find_right(key);
    if (it == end_right()) {
      throw std::out_of_range("No such element");
    }
    return *it.flip();
  }

  // Возвращает противоположный элемент по элементу
  // Если элемента не существует, добавляет его в bimap и на противоположную
  // сторону кладет дефолтный элемент, ссылку на который и возвращает
  // Если дефолтный элемент уже лежит в противоположной паре - должен поменять
  // соответствующий ему элемент на запрашиваемый (смотри тесты)
  const right_t& at_left_or_default(const left_t& key) 
    requires std::default_initializable<right_t>
  {
    auto pos = left_set.insert_pos(key);
    if (pos) {
      right_t element{};
      auto pair_pos = right_set.insert_pos(element);
      if (pair_pos) {
        auto node = new node_t(key, std::move(element));
        left_set.insert(pos, *node);
        auto it = right_set.insert(pair_pos, *node);
        return it->right;
      } else {
        auto right = static_cast<right_node*>(pair_pos.node());
        auto node = static_cast<node_t*>(right);
        auto left = static_cast<left_node*>(node);
        left_set.erase(typename left_set_t::iterator(left));
        try {
          auto new_pos = left_set.insert_pos(key);
          node->left.~left_t();
          std::construct_at(std::addressof(node->left), key);
          left_set.insert(new_pos, *node);
        } catch (...) {
          right_set.erase(typename right_set_t::iterator(right));
          delete node;
          throw;
        }
        return node->right;
      }
    } else {
      return from_tag<left_tag>(pos.node())->right;
    }
  }

  const left_t& at_right_or_default(const right_t& key)
    requires std::default_initializable<left_t>
  {
    auto pos = right_set.insert_pos(key);
    if (pos) {
      left_t element{};
      auto pair_pos = left_set.insert_pos(element);
      if (pair_pos) {
        auto node = new node_t(std::move(element), key);
        right_set.insert(pos, *node);
        auto it = left_set.insert(pair_pos, *node);
        return it->left;
      } else {
        auto left = static_cast<left_node*>(pair_pos.node());
        auto node = static_cast<node_t*>(left);
        auto right = static_cast<right_node*>(node);
        right_set.erase(typename right_set_t::iterator(right));
        try {
          auto new_pos = right_set.insert_pos(key);
          node->right.~right_t();
          std::construct_at(std::addressof(node->right), key);
          right_set.insert(new_pos, *node);
        } catch (...) {
          left_set.erase(typename left_set_t::iterator(left));
          delete node;
          throw;
        }
        return node->left;
      }
    } else {
      return from_tag<right_tag>(pos.node())->left;
    }
  }

  // lower и upper bound'ы по каждой стороне
  // Возвращают итераторы на соответствующие элементы
  // Смотри std::lower_bound, std::upper_bound.
  left_iterator lower_bound_left(const left_t& left) const {
    return left_set.lower_bound(left);
  }

  left_iterator upper_bound_left(const left_t& left) const {
    return left_set.upper_bound(left);
  }

  right_iterator lower_bound_right(const right_t& left) const {
    return right_set.lower_bound(left);
  }

  right_iterator upper_bound_right(const right_t& left) const {
    return right_set.upper_bound(left);
  }

  // Возващает итератор на минимальный по порядку left.
  left_iterator begin_left() const {
    return left_set.begin();
  }

  // Возващает итератор на следующий за последним по порядку left.
  left_iterator end_left() const {
    return left_set.end();
  }

  // Возващает итератор на минимальный по порядку right.
  right_iterator begin_right() const {
    return right_set.begin();
  }

  // Возващает итератор на следующий за последним по порядку right.
  right_iterator end_right() const {
    return right_set.end();
  }

  // Проверка на пустоту
  bool empty() const {
    return sz == 0;
  }

  // Возвращает размер бимапы (кол-во пар)
  std::size_t size() const {
    return sz;
  }

  // Операторы сравнения
  friend bool operator==(const bimap& lhs, const bimap& rhs) {
    if (lhs.size() != rhs.size()) {
      return false;
    }
    for (auto lit = lhs.left_set.begin(), rit = rhs.left_set.begin(); lit != lhs.left_set.end(); ++lit, ++rit) {
      if (!lhs.equal(*lit, *rit)) {
        return false;
      }
    }
    return true;
  }

  friend bool operator!=(const bimap& lhs, const bimap& rhs) {
    return !(lhs == rhs);
  }

private:
  template <typename L, typename R>
  left_iterator perform_insert(L&& left, R&& right) {
    auto left_pos = left_set.insert_pos(left);
    if (left_pos) {
      auto right_pos = right_set.insert_pos(right);
      if (right_pos) {
        auto *new_node = new node_t(std::forward<L>(left), std::forward<R>(right));
        left_iterator it = left_set.insert(left_pos, *new_node);
        right_set.insert(right_pos, *new_node);
        ++sz;
        return it;
      }
    }
    return end_left();
  }
};
