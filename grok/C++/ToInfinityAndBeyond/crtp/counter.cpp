/*
 *  Illustrating Curiously Recurring Template Pattern
 */
#include <iostream>

using namespace std;

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

class Dog : public counter<Dog> {
public:
  void speak() { cout << "Bark, Bark" << endl; }
};

class Robot : public counter<Robot> {
public:
  void speak() { cout << "Danger Will Robinson! Danger!" << endl; }
};

class Goat : public counter<Goat> {};

void callWalter() {
  Dog walter;

  cout << "Entered callWalter\n";
  cout << "Number of Dogs created: " << walter.getCreated() << endl;
  cout << "Number of Dogs alive: " << walter.getAlive() << endl;
  cout << "Leaving callWalter\n";
}

int main() {
  Dog fido;
  Dog flash;
  Robot robbie;
  Robot robot;
  Goat nanny;
  Goat billy;

  cout << "The robot says: ";
  robot.speak();

  cout << "Number of Robots created: " << robot.getCreated() << endl;
  cout << "Number of Robots alive: " << robbie.getAlive() << endl;
  cout << "Number of Dogs created: " << flash.getCreated() << endl;
  cout << "Number of Dogs alive: " << fido.getAlive() << endl;

  callWalter();

  cout << "Number of Dogs created: " << fido.getCreated() << endl;
  cout << "Number of Dogs alive: " << flash.getAlive() << endl;

  cout << "Nanny say there were " << nanny.getCreated() << " goats created."
       << endl;
  cout << "Billy say there are " << billy.getAlive() << " goats alive." << endl;

  cout << "Fido speaks!" << endl;
  fido.speak();

  cout << "Creating gort from robbie" << endl;
  Robot gort = robbie;
  cout << "Gort say, number Robots created: " << gort.getCreated() << endl;
  cout << "Robbie says, number of Robots alive: " << robbie.getAlive() << endl;

  return 0;
}
