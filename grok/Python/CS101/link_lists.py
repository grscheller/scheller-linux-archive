# Singularly linked list with Python
# see: https://realpython.com/linked-lists-python/

class Node:
    def __init__(self, data):
        self.data = data
        self.next = None

    def __repr__(self):
        return self.data

class LinkedList:
    def __init__(self):
        self.head = None

    def append(self, data):
        node = Node(data)
        node.next = self.head
        self.head = node
