# Copyright 2024 Geoffrey R. Scheller (GitHub: grscheller)
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Derived from Udacity's "AI Programming with Python" exercises
#
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
