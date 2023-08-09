# FIFO Queue
#
#   Using https://realpython.com/linked-lists-python/ as a guide.
#
#   Notes:
#     1. See collections.deque for an implementation of a linked list which
#        can access, insert, or remove elements from the beginning or end
#        of a list with constant O(1) performance.
#     2. Implements a circular array using a Python array.
#

class Queue:
    def __init__(self, capacity=2, dataList=None):
        self.front = self.count = 0
        self.rear = capacity - 1
        self.queue = [None] * capacity
        self.capacity = capacity
        if dataList is not None:
            pass
#            dataListReversed = dataList.copy()
#            dataListReversed.reverse()
#            for data in dataListReversed:
#                node = Node(data, self.head)
#                self.head = node
#                self.count += 1

    def isFull(self):
        return self.count == self.capacity

#    def __iter__(self):
#        node = self.head
#        while node is not None:
#            yield node.data
#            node = node.next
#
#    def __repr__(self):
#        dataListStrs = []
#        for data in self:
#            dataListStrs.append(repr(data))
#        dataListStrs.append("None")
#        return " -> ".join(dataListStrs)
#
    def enqueue(self, data):
        if self.isFull():
            print("There is no room in the inn for", repr(data))   # reimplement to resize
            return
        self.rear = (self.rear + 1) % self.capacity
        self.queue[self.rear] = data
        self.count += 1

    def dequeue(self):
        if self.count == 0:
            return None
        else:
            data = self.queue[self.front]
            self.queue[self.front] = None
            self.front = (self.front + 1) % self.capacity
            self.count -= 1
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
    print(q1.dequeue())
    print(q1.dequeue())
    print(q1.dequeue())

if __name__ == "__main__":
    test_it()
