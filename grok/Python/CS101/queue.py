"""FIFO queue

Module implementing a FIFO Queue with a Python List based circular array
"""

# Notes:
#
#   1. See collections.deque for an implementation of a linked list which
#      can access, insert, or remove elements from the beginning or end
#      of a list with constant O(1) performance.
#   2. Implements a circular array using a Python array.
#   3. In Python, functions arguments are all done by value...but all
#      values are references to objects! "Variables" are just labels we
#      put on objects. An object can have multiple, or no, labels on it.
#   4. Everything in Python is an object, or is a piece of compiled
#      code that acts like an object. These "objects" are either built
#      into the interpreter ("builtins"), are written with Python in mind
#      and use the Python FFI (needing some knowledge of Python internals),
#      or uses a language specific interface package like ctypes.
#   5. Classes are objects too! They are instantiations of
#      "meta-classes". Of course, the turtles can't go down forever.
#      (TL/DR) The Python class keyword is actually syntactic surgar
#      for the Python type() builtin function. Not only does type display
#      the types of objects, it is actually the meta-class at the bottom of
#      the turtle pile. Type is its own meta-class. Actually this is something
#      you can't reproduce in pure Python, it is done by cheating a bit at
#      the implementation level. Python 3 has other syntactic sugars for
#      defining and dealing with meta-classes. See
#      https://stackoverflow.com/questions/100003/what-are-metaclasses-in-python
#

__all__ = ['Queue']
__author__ = "Geoffrey R. Scheller"

class Queue:
    """
    First In, First Out (FIFO) queue datastructure. The queue will
    resize itself larger as needed.
    """
    def __init__(self, size=2, data=None):
        """
        Parameters:
            size (int): minimum initial capacity of queue
            data (List): data to prepopulate the queue
        """
        if data is None:
            capacity = max(size, 2)
            self._capacity = capacity
            self._count = 0
            self._front = 0
            self._rear = capacity - 1
            self._queue = [None]*capacity
        else:
            dataLength = len(data)
            capacity = max(size, dataLength, 2)
            self._capacity = capacity
            self._count = dataLength
            self._front = 0
            self._rear = dataLength - 1
            self._queue = data + [None]*(capacity - dataLength)

    def isFull(self):
        """Returns true if queue is at full compacity."""
        return self._count == self._capacity

    def count(self):
        """Returns current number of objects in queue"""
        return self._count

    def __iter__(self):
        """Iterator yielding data stored in queue, does not consume data."""
        if self._count > 0:
            pos = (self._front - 1) % self._capacity
            while pos != self._rear:
                pos = (pos + 1) % self._capacity
                yield self._queue[pos]

    def __repr__(self):
        """Display enqueued data"""
        dataListStrs = []
        for data in self:
            dataListStrs.append(repr(data))
        return "|< " + " <- ".join(dataListStrs) + " <|"

    def enqueue(self, data):
        """Add data at rear of queue"""
        if self.isFull():
            if self._front > self._rear:
                frontPart = self._queue[self._front:]
                rearPart = self._queue[:self._rear+1]
            else:
                frontPart = self._queue
                rearPart = []
            self._queue = frontPart + rearPart + [None]*(self._capacity)
            self._capacity *= 2
            self._front = 0
            self._rear = self._count - 1
        self._rear = (self._rear + 1) % self._capacity
        self._queue[self._rear] = data
        self._count += 1

    def dequeue(self):
        """Pop data off front of queue"""
        if self._count == 0:
            return None
        else:
            data = self._queue[self._front]
            self._queue[self._front] = None
            self._front = (self._front + 1) % self._capacity
            self._count -= 1
            return data

def test_it ():
    """Poor man's test suite"""
    q1 = Queue()
    q1.enqueue('a')
    q1.enqueue('b')
    out1 = q1.dequeue()
    print(out1)
    print(q1.dequeue())
    q1.enqueue('c')
    q1.enqueue('d')
    q1.enqueue('e')
    print(q1.dequeue())
    q1.enqueue('f')
    print(q1)
    print(q1.dequeue())
    print(q1)
    print(q1.dequeue())
    print(q1)
    q1.enqueue('g')
    print(q1.dequeue())
    print(q1)
    print(q1.dequeue())
    print(q1)
    print(q1.dequeue())
    print(q1)

if __name__ == "__main__":
    test_it()
