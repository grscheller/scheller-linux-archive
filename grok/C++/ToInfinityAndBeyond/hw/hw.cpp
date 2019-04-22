/*
 *  Traditional K&R Hello World program done in C++.
 */
#include <iostream>
#include <string>
#include <thread>

auto hw = "  Hello World!";         // Type inference (char* in this case).
std::string gw {"Goodbye World!"};  // Uniform Initialization.

void hello()
{
    using namespace std;   // Use of namespaces in a lexiconic scope.

    cout << hw+2 << endl;  // Can still "fiddle the bits" with
                           // pointers into character arrays
}

int main()
{
    std::thread t(hello);
    t.join();
    std::cout << gw << '\n';  // can't do pointer arithmetic on object gw 

    return 0;
}

/*
 *  Notes:
 *
 *  1. Uniform Initialization turns off C data narrowing misfeature.
 *  2. Unlike ANSI C, main's prototype indicates that it takes no arguments.
 *  3. On older C++ compilers, like GCC 4.8.5, combining uniform
 *     initialization with type inference via auto keyword, will
 *     instantiate hw as a std::basic_string and not a char*.
 *
 */

