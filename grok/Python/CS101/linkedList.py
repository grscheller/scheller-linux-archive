# Singularly linked list with Python
#
#   Using https://realpython.com/linked-lists-python/ as a guide.
#
#   Notes:
#     1. Using None to represent nothing, therefore None is not a
#        legitimate data value.
#     2. Python has no automatic type coersion, hence the use of
#        the str & repr functions.
#          __repr__() gives the formal string representation for developers
#          __str__() gives an informal string representation for users
#     3. Prefer ''.join() forms over a = a + b or a += b. This will ensure
#        that concatenation occurs in linear time across various
#        python implementations.
#     4. Module level “dunders” such as __all__, __author__, __version__, etc.
#        should be placed after the module docstring but before any import
#        statement.
#     5. This file is an example of a python "module". A Python package is
#        is a directory consisting of Python modules and packages. A package
#        contains an __init__.py file tying everything together..
#

__all__ = ['LinkedList']
__author__ = "Geoffrey R. Scheller"

class _Node:
    def __init__(self, data, nodeNext=None):
        self._data = data
        self._next = nodeNext

class LinkedList:
    def __init__(self, dataList=None):
        self.head = None
        self.count = 0
        if dataList is not None:
            dataListReversed = dataList.copy()
            dataListReversed.reverse()
            for data in dataListReversed:
                node = _Node(data, self.head)
                self.head = node
                self.count += 1

    def __iter__(self):
        node = self.head
        while node is not None:
            yield node._data
            node = node._next

    def __repr__(self):
        dataListStrs = []
        for data in self:
            dataListStrs.append(repr(data))
        dataListStrs.append("None")
        return " -> ".join(dataListStrs)

    def push(self, data):
        node = _Node(data, self.head)
        self.head = node
        self.count += 1

    def pop(self):
        if self.head is None:
            return None
        else:
            data = self.head._data
            self.head = self.head._next
            self.count -= 1
            return data

def test_it ():
    ll = LinkedList([1, 2, 3])

    print("initial list:", ll, "\ninitially with", ll.count, "elements")
    print("\npush 10, 7, and then 13\n")
    ll.push(10)
    ll.push(7)
    ll.push(13)
    print("modified list:", ll, "\nnow with", ll.count, "elements")

    forty_two = 42
    print("\npop: %d\npush: %d" % (ll.pop(), forty_two))
    ll.push(forty_two)

    print("\ninterate over list data without consuming it")
    n = 0
    for data in ll:
        n += 1
        print("%d -> %s" % (n, repr(data)))
    print("That's all folkes! no more data to show!")

    print("\ntest mixed data types, float, string, tuple")
    ll = LinkedList([3.14159, "Hello Python", (42, "Excelsior!")])
    print(ll)

if __name__ == "__main__":
    test_it()
