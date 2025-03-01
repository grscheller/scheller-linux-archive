#!/usr/bin/env python
#
#  In parallel as completed
#

import asyncio


async def foo():
    await asyncio.sleep(2)
    return "Foo done"


async def bar():
    await asyncio.sleep(1)
    return "Bar done"


async def main():
    tasks = [foo(), bar()]
    for result in asyncio.as_completed(tasks):
        print(await result)


if __name__ == "__main__":
    asyncio.run(main())
