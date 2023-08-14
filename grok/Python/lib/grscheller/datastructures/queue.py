"""FIFO queue

Module implementing a FIFO Queue with a Python List based circular array
"""
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
