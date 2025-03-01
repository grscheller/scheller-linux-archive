#!/usr/bin/env python
#
# Awaitable object
#
# Note use of yield.
#

import asyncio

async def foo():
    await asyncio.sleep(2)
    print("Hello from foo!")
async def bar():
    await asyncio.sleep(1)
    print("Hello from bar!")
async def main():
    task1 = asyncio.create_task(foo())
    task2 = asyncio.create_task(bar())
    await task1
    await task2

if __name__ == "__main__":
    asyncio.run(main())
