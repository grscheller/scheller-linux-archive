#!/usr/bin/env python

from __future__ import annotations
from typing import Any

__all__ = ['Animal', 'Dog', 'Cat']

class Animal:
    def __init__(self, name: str, age: int):
        self.name = name
    def speak(self) -> None:
        print('I am an animal.')
    def my_name(self) -> str:
        return 'Animal: ' + self.name

class Dog(Animal):
    def speak(self) -> None:
        print('Woof.')
    def sleep(self) -> None:
        print('zzz....')
    def my_name(self) -> str:
        return 'Dog: ' + self.name

class Cat(Animal):
    def speak(self) -> None:
        print('Meow.')
    def sleep(self) -> None:
        print('purr....')
    def my_name(self) -> str:
        return 'Cat: ' + self.name

