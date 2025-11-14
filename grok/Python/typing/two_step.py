# Modified from a tangled piece of Google AI generated code.
#
# Goal is to be able to create a class capable of holding a subclass
# of another class that does not exist yet, with no mypy errors.
#
# Prints to screen:
#   A1: runtime type is <class '__main__.CA1'>
#   C1: runtime type is <class '__main__.C2'>
#
# This was used to sort out boring-math-abstract-algebra.
#
from abc import ABC, abstractmethod
from typing import Self


class C1:
    def speak(self) -> None:
        print(
            f"C1: runtime type is {type(self)}",
        )


class A1(ABC):
    def __init__(self, c: C1) -> None:
        self._c = c

    def say(self) -> None:
        print(f"A1: runtime type is {type(self)}")
        self._c.speak()

    @abstractmethod
    def create(self) -> Self: ...


class CA1(A1):
    def create(self) -> Self:
        return type(self)(self._c)


class C2(C1):
    pass


if __name__ == "__main__":
    instance = CA1(C2())
    instance.say()
