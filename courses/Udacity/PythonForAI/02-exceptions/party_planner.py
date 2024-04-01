#!/usr/bin/env python

def party_planner(cookies, people):
    # Verify user input and perform calculation
    #   grs - warning: "happy path" is exceptional
    #   grs - input always returns a string
    #   grs - "cast" user input carefully, they might be smarter than you
    validated = False
    while not validated:
        try:
            cookies = int(cookies)
            if cookies < 0:
                raise ValueError
        except ValueError:    
            cookies = input("Enter a whole number of cookies: ")
        try:
            people = int(people)
            if people <= 0:
                raise ValueError
        except ValueError:
            people = input("Enter the number of people, at least one: ")

        if type(cookies) == int and type(people) == int:
            validated = True

    num_each = cookies // people
    leftovers = cookies % people

    return(num_each, leftovers, cookies, people)

def main():
    # The main code block is below; do not edit this
    #   grs - fixed bug line party_planner() called
    #   grs - isolating exceptional thinking to one function
    #   grs - remove "party voice" from actual code, it belongs in the UI
    loop = True
    while loop:

        cookies_ui = input('How many cookies are you baking? ')
        people_ui = input('How many people are attending? ')

        cookies_each, leftovers, cookies, people = party_planner(cookies_ui, people_ui)
        msg = "\nLet's party! We'll have {} people attending, they'll each get to eat {} cookies, and we'll have {} left over."
        print(msg.format(people, cookies_each, leftovers))

        ans = input('\nWould you like to party more? ([y] or n) ') + 'y'
        loop = True if (ans.strip()+'y')[0].lower() == 'y' else False

if __name__ == "__main__":
    # Wrap main in an exception handler
    #   grs - just stumbled upon this pattern
    #   grs - good for truly exceptional events
    try:
        main()
    except KeyboardInterrupt as e:
        print("\nGoodbye!")
    except Exception as error:
        print(f"\n\nUnexpected {error=}")
    else:
        print("Goodbye!")
