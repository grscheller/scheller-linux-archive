""" Module: func_tools

    Poor man's version of itertools

"""

__author__ = "Geoffrey Scheller"

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
