#!/usr/bin/env python

"""Test syntax for *args and **kwargs"""

import sys

def prt_name_scores(name, *nums):
    """Print name and numbers

       Example of a function that can take
       a variable number arguments"""
    print(f"Name: {name}")
    for num in nums:
        print(num)
    print()

def prt_names_and_ages(**name_age_dict):
    """Print name and associated age

       Example of a function that takes
       key-value pairs"""
    for name, age in name_age_dict.items():
        print(f"{name} is {age} years old")
    print()

def main():
    """Main function

       Best practice is to use a main function and
       not write a script at the module level"""

    # Minimal argument processing
    args = sys.argv[1:]

    # Can take a variable number of arguments
    prt_name_scores("Fred", 91, 98, 100, 98)

    scores = 100, 89, 96, 98
    more_scores = [90, 99]
    more_scores.append(100)

    # Can "splat" in data structures
    prt_name_scores("Jane", *scores, 42, *more_scores, *args)

    # Using key-value dictionary pairs
    prt_names_and_ages(Abbot=35, Costello=34)

    # Using single dictionary
    stooges = {
        'Moe'   : 42,
        'Larry' : 45,
        'Shemp' : 43
        }
    stooges["Curlie"] = 40
    prt_names_and_ages(**stooges)

    # Using two dictionaries
    gang = {}
    gang['Fred'] = 20
    gang['Dafney'] = 19
    gang['Thema'] = 24
    gang['Shaggie'] = 32
    prt_names_and_ages(**gang, **stooges)

# Canonical boiler plate
if __name__ == "__main__":
    main()
