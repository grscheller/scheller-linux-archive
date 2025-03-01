#!/usr/bin/env python
#
#  In parallel as completed
#

import asyncio
import aiohttp
from bs4 import BeautifulSoup


async def fetch_and_parse_url(url):
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            page_content = await response.text()
            soup = BeautifulSoup(page_content, "html.parser")
            title = soup.title.string
            return f"Title of {url}: {title}"


async def main():
    urls = [
        "https://example.com",
        "https://python.org",
        "https://google.com",
    ]
    tasks = [fetch_and_parse_url(url) for url in urls]
    results = await asyncio.gather(*tasks)
    for result in results:
        print(result)


if __name__ == "__main__":
    asyncio.run(main())
