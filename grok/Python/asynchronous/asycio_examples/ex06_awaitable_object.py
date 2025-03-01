#!/usr/bin/env python
#
# Awaitable object
#
# Note use of yield.
#

import asyncio


class MyAwaitable:
    def __await__(self):
        yield
        return 42


async def main():
    result = await MyAwaitable()
    print(result)


if __name__ == "__main__":
    asyncio.run(main())
