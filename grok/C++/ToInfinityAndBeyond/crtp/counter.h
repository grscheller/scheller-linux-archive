/*
 *  Illustrating Curiously Recurring Template Pattern
 */

#ifndef COUNTER_H
#define COUNTER_H

template <typename T> struct counter {
  static int objects_created;
  static int objects_alive;

  counter() {
    ++objects_created;
    ++objects_alive;
  }

  counter(const counter &) {
    ++objects_created;
    ++objects_alive;
  }

  int getCreated() { return objects_created; }

  int getAlive() { return objects_alive; }

protected:
  ~counter() { --objects_alive; }
};
template <typename T> int counter<T>::objects_created{0};
template <typename T> int counter<T>::objects_alive{0};

#endif
