/*
 *  Traditional K&R Hello World program done in C++.
 *
 *  Illustrates:
 *
 *  1. Type infarence
 *  2. IOSTREAM library
 *  3. Still lets you "fiddle the bits" with
 *     pointers to chracter arrays
 *  4. Use of namespaces
 *  5. Unlike ANSI C, main's prototype indicates
 *     that it takes no arguments.
 */
#include <iostream>

int main() {

    auto hw = "  Hello World!";
    std::cout << hw+2 << "\n";

    return 0;

}
