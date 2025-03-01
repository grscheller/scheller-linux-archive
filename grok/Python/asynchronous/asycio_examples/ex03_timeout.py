#!/usr/bin/env python
#
# Timeout after 3 seconds
#

import asyncio


async def slow_operation():
    await asyncio.sleep(5)
    return "Operation complete!"


async def main():
    try:
        result = await asyncio.wait_for(slow_operation(), timeout=3)
        print(result)
    except asyncio.TimeoutError:
        print("Operation timed out!")


if __name__ == "__main__":
    asyncio.run(main())
