#!/usr/bin/env python

from typing import Any
from pets.animals import Animal, Dog, Cat

flash = Dog('Flash', 12)
walter = Dog('Walter', 14)
kitty = Cat('Kitty', 18)
phoenix = Cat('Phoenix', 8)

animals: list[Animal] = []

animals.append(flash)
animals.append(kitty)
animals.append(walter)
animals.append(phoenix)

for animal in animals:
    animal.speak()

for animal in animals:
    print(animal.name)

# dogs: list[Dog] = [flash, kitty]
dogs: list[Dog] = [flash, walter]
cats: list[Cat] = [kitty, phoenix]

# Dog <: Animal
# Dog.speak: Callable[[], None] :> Animal.speak: Callable[[], None]
