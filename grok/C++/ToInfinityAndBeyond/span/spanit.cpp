#include <iostream>
#include <vector>
#include <array>
#include <span>

using namespace std;

void f(span<int> seq)
{
    cout << seq.size() << ": ";
    for (auto &x: seq) cout << x << ' ';
    cout << endl;
}

void g(const auto& seq)
{
    for (auto &x: seq) cout << x << ' ';
    cout << endl;
}

int main(void)
{
    int a[] {1, 2, 42, 314159};
    constexpr int b[] {10, 9, 8, 8, 7, 6, 5, 4, 3*4 - 1};
    vector<int> vec {3, 1, 4, 1, 5, 9};
    array<int, 7> arr {30, 14, 57, 99, 100, -11, 5};
    f(vec);
    f(arr);
    f(a);

    auto bs = span(b);
    cout << bs.size() << ": ";
    for (auto &x: bs) cout << x << ' ';
    cout << "\n\n";

    g(vec);
    g(arr);
    g(a);
    g(b);
    cout << endl;

    return 0;
}
