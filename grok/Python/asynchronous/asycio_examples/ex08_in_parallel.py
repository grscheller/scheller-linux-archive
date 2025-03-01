#!/usr/bin/env python
#
# Tasks in parallel
#

import asyncio


async def square(x):
    await asyncio.sleep(1)
    return x * x


async def cube(x):
    await asyncio.sleep(2)
    return x * x * x


async def main():
    results = await asyncio.gather(square(3), cube(3))
    print("Square:", results[0])
    print("Cube:", results[1])


if __name__ == "__main__":
    asyncio.run(main())
