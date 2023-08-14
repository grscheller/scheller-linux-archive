"""LIFO stack

Module implementing a LIFO stack using a singularly linked list.
"""
__all__ = ['Stack']
__author__ = "Geoffrey R. Scheller"

class _Node:
    """
    Nodes making up the linked list. None represents a "nothing" and indicates
    the end of the list. Therefore, None is not a ligitamate data value.
    """
    def __init__(self, data, nodeNext=None):
        self._data = data
        self._next = nodeNext

class Stack:
    """
    Last In, First Out (LIFO) stack datastructure. The stack is implemented
    as a singularly linked list of nodes. The stack points to either the first
    node in the list, or to None.
    """
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
        """Iterator yielding data stored in the stack, does not consume data."""
        node = self.head
        while node is not None:
            yield node._data
            node = node._next

    def __repr__(self):
        """Display the data in the stack."""
        dataListStrs = []
        for data in self:
            dataListStrs.append(repr(data))
        dataListStrs.append("None")
        return " -> ".join(dataListStrs)

    def push(self, data):
        """Push data onto top of stack."""
        node = _Node(data, self.head)
        self.head = node
        self.count += 1

    def pop(self):
        """Pop data off of top of stack."""
        if self.head is None:
            return None
        else:
            data = self.head._data
            self.head = self.head._next
            self.count -= 1
            return data

def test_it ():
    """Poor man's test suite"""
    ll = Stack([1, 2, 3])

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
    ll = Stack([3.14159, "Hello Python", (42, "Excelsior!")])
    print(ll)

if __name__ == "__main__":
    test_it()
