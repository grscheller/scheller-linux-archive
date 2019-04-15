/*
 *  Illustrating Curiously Recuring Template Pattern
 */
#include <iostream>

template <typename T>
struct counter
{
    static int objects_created;
    static int objects_alive;

    counter()
    {
        ++objects_created;
        ++objects_alive;
    }

    counter(const counter&)
    {
        ++objects_created;
        ++objects_alive;
    }

  protected:
    ~counter()
    {
        --objects_alive;
    }
};
template <typename T> int counter<T>::objects_created(0);
template <typename T> int counter<T>::objects_alive(0);

class Dog : counter<Dog>
{

};

class Robot : counter<Robot>
{

};

int main()
{
    Dog fido;
    Dog flash;
    Robot robbie;

    using namespace std;

    // cout << "Number of Dogs created: " << Dog.objects_created << endl;
    // cout << "Number of Dogs alive: "   << fido.objects_alive   << endl;

    return 0;
}

/*
 *  Notes:
 *
 *  1. 
 */

