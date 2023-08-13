# FIFO Queue
#
#   Using https://realpython.com/linked-lists-python/ as a guide.
#
#   Notes:
#     1. See collections.deque for an implementation of a linked list which
#        can access, insert, or remove elements from the beginning or end
#        of a list with constant O(1) performance.
#     2. Implements a circular array using a Python array.
#     3. Functions arguments are all done by value...but in Python all
#        values are references to objects. "Variables" are just labels we
#        put on objects. An object can have multiple, or no, labels on it.
#     4. (TL;DR) Everything in Python is an object, or is a piece of C
#        code that acts like an object. These "objects" are either built
#        into the interpreter "builtins", are written with Python in mind
#        and use Python FFI (needing some knowledge of Python internals),
#        or uses a language specific interface package like ctypes.
#     5. (TL/DR) Classes are objects too! They have meta-classes. Of course,
#        the turtles can't go down forever.
#

__all__ = ['Queue']
__author__ = "Geoffrey R. Scheller"

class Queue:
    def __init__(self, queueSize=2, dataList=None):
        if dataList is None:
            capacity = max(queueSize, 2)
            self._capacity = capacity
            self._count = 0
            self._front = 0
            self._rear = capacity - 1
            self._queue = [None]*capacity
        else:
            dataLength = len(dataList)
            capacity = max(queueSize, dataLength, 2)
            self._capacity = capacity
            self._count = dataLength
            self._front = 0
            self._rear = dataLength - 1
            self._queue = dataList + [None]*(capacity - dataLength)

    def isFull(self):
        return self._count == self._capacity

    def count(self):
        return self._count

    def __iter__(self):
        if self._count > 0:
            pos = (self._front - 1) % self._capacity
            while pos != self._rear:
                pos = (pos + 1) % self._capacity
                yield self._queue[pos]
                
    def __repr__(self):
        dataListStrs = []
        for data in self:
            dataListStrs.append(repr(data))
        return "|< " + " <- ".join(dataListStrs) + " <|"

    def enqueue(self, data):
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
        if self._count == 0:
            return None
        else:
            data = self._queue[self._front]
            self._queue[self._front] = None
            self._front = (self._front + 1) % self._capacity
            self._count -= 1
            return data

def test_it ():
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
