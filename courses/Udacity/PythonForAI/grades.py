from __future__ import annotations

from typing import Iterator

def parse_input(msg: str, tp: type=str, sep: str=',') -> Iterator:
    return map(lambda x: tp(x.strip()), input(msg).split(sep))

msg = 'Enter names separated by commas: '
names = tuple(map(lambda x: x.title(), parse_input(msg)))

msg = 'Enter grades separated by commas: '
grades = tuple(parse_input(msg, int))

msg = 'Enter number of missed assignments separated by commas: '
assignments = tuple(parse_input(msg, int))

message = '''\nHi {},\n\nThis is a reminder that you have {} assignments left to
submit before you can graduate. You're current grade is {} and can increase
to {} if you submit all assignments before the due date.\n'''

for name, assignment, grade in zip(names, assignments, grades):
    print(message.format(name, int(assignment), int(grade), int(grade) + int(assignment)*2))
