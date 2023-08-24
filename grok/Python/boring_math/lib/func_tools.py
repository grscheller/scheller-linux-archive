# Copyright 2016-2023 Geoffrey R. Scheller
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

"""Boring Math Functional Library

Library of functional programming inspired tools. A poor man's version of
itertools.
"""

__author__ = "Geoffrey R. Scheller"

import sys

__all__ = ['drop', 'take', 'drop_while', 'take_while']


def drop(num, iterator):
    """Drop the first n elements on an iterator"""

    try:
        for _ in range(num):
            next(iterator)
    except StopIteration:
        return iterator

    return iterator


def take(num, iterator):
    """Iterator returning next n elements from iterator"""

    for _ in range(num):
        try:
            next_val = next(iterator)
        except StopIteration:
            return
        yield next_val


def drop_while(pred, iterator):
    """Drop iterator elements while predicate true"""

    for next_val in iterator:
        if pred(next_val):
            continue
        yield next_val


def take_while(pred, iterator):
    """Take iterator elements while predicate true"""

    for next_val in iterator:
        if pred(next_val):
            yield next_val
        else:
            break


if __name__ == '__main__':
    sys.exit(0)
