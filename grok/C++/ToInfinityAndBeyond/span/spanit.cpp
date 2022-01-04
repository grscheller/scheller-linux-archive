#include <iostream>
#include <vector>
#include <array>
#include <span>

using namespace std;

void f(span<int> s)
{
    cout << s.size() << ": ";
    for (auto &x: s) cout << x << ' ';
    cout << endl;
}

int main(void)
{
    int a[]{1, 2, 42};
    vector<int> vec{3, 1, 4, 1, 5, 9};
    array<int, 7> arr{30, 14, 57, 99, 100, -11, 5};
    f(a);
    f(vec);
    f(arr);

    return 0;
}
