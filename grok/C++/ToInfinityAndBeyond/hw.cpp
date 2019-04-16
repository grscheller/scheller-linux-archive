/*
 *  Traditional K&R Hello World program done in C++.
 */
#include <iostream>
#include <thread>

auto hw {"  Hello World!"};
auto gw {"Goodbye World!"};

void hello()
{
    using namespace std;

    cout << hw+2 << endl;
}

int main()
{
    std::thread t(hello);
    t.join();
    std::cout << gw << '\n';

    return 0;
}

/*
 *  Notes:
 *
 *  1. Type infarence via auto keyword
 *  2. IOSTREAM library
 *  3. Uniform Initialization turns off Fortran-like automatic
 *     data converion.
 *  4. Use of namespaces
 *  5. Still lets you "fiddle the bits" with
 *     pointers to chracter arrays
 *  6. Unlike ANSI C, main's prototype indicates
 *     that it takes no arguments.
 *  7. Function overloading makes << easy to deal with
 *  8. Built in std::thread library even with base compile
 *  9. Currently not specifying any sort of "standard"
 * 10. Need to compile with
 *       g++ -Wall -pthread hw.cpp -o hw
 *     otherwise linker error with pthread_create
 *     undefined.  Got the -pthread from the pthread_create 
 *     Linux, not POSIX, manpage.
 */

