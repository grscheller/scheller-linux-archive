#!/usr/bin/env python
#
# Fetch multiple URLS simultaneously.
#

import asyncio
import aiohttp


async def fetch_url(url):
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            return await response.text()


async def main():
    urls = ['https://example.com', 'https://python.org', 'https://google.com']
    tasks = [fetch_url(url) for url in urls]
    results = await asyncio.gather(*tasks)
    for url, content in zip(urls, results):
        print(f"Fetched {len(content)} bytes from {url}")


if __name__ == "__main__":
    asyncio.run(main())
