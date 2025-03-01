#!/usr/bin/env python
#
# Timeout while attempting to "fetch" data
#

import asyncio


async def fetch_data():
    try:
        await asyncio.sleep(2)
        result = 10 / 0  # This raises a ZeroDivisionError
        print(result)
    except ZeroDivisionError as e:
        return f"Error: {e}"
    return "Data fetched successfully!"


async def main():
    result = await fetch_data()
    print(result)


if __name__ == "__main__":
    asyncio.run(main())
