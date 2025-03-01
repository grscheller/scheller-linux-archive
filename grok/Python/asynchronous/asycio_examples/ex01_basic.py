#!/usr/bin/env python
#
# Pause execution main thread until a specific tasks are complete.
#
# An "async def" is not a function but something a little different.
# Evaluated in something called a "coroutine" and is some sort of "thunk."
#
#    $ ./ex01_basic.py
#    Starting main at 1740779503.2661197
#    Main thread done sleeping at 1740779508.2664025
#    Jason: created 1740779503.2661579; started 1740779508.2668045; awoken 1740779509.2682705
#    George: created 1740779503.266146; started 1740779508.2665784; awoken 1740779511.2696807
#    Last await called at 1740779511.2698698
#
# Note: The arguments passed to run_it were strictly evaluated
#       but the run_it "thunk" was non-strictly evaluated.
#
# Note: Both run_it and main were defined as `async def` and
#       async is NOT a decorator. The first `await` call in main
#       is deferring main coroutine execution until after task1
#       is done.
#

import asyncio
from time import sleep, time


async def run_it(name: str, created: str, nap: int) -> None:
    started = str(time())
    await asyncio.sleep(nap)
    awoken = str(time())
    msg = "{}: created {}; started {}; awoken {}"
    print(msg.format(name, created, started, awoken))


async def main():
    print("Starting main at {}".format(str(time())))

    task1 = asyncio.create_task(run_it("George", str(time()), 3))
    task2 = asyncio.create_task(run_it("Jason", str(time()), 1))

    sleep(5)
    print("Main thread done sleeping at", end=" ")
    print(str(time()))

    await task1
    await task2
    print("Last await called at", end=" ")
    print(str(time()))


if __name__ == "__main__":
    asyncio.run(main())
