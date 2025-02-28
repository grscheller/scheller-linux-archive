# Asynchronous code in Python

* Do more at once
  * asyncio - for handling many IO bound tasks 
  * threads - GIL and post-GIL
* Do faster
  * multiprocessing - multiple processes
  * C/Cython - use C under-the-hood
  * Numpy - use C under-the-hood without learning it

## Subprojects

### Web examples

From
[Daniel Wu](https://medium.com/@danielwume/an-in-depth-guide-to-asyncio-and-await-in-python-059c3ecc9d96#:~:text=The%20await%20keyword%20is%20used,awaited%20coroutine%20is%20in%20progress.)
on medium.

## Notes

### What is a Python `async def`

From a 5 year old
[Reddit post](https://www.reddit.com/r/learnpython/comments/hdhuyc/eli5_async_def_what_it_does_and_how_it_works/?rdt=33070):

`async def` means the function is actually a coroutine. All this means
essentially is that the function can be “paused” and resume the rest of
its execution at a later time. You tell the coroutine where it can be
paused by using the await keyword, where the await keyword is followed
by an awaitable object. With await you’re essentially saying pause until
the awaitable object is done. So it quite literally means await the
completion of the object.

This “pausing” functionality is not actually useful unless you run your
async code within the context of an event loop (one implementation of
which is provided through the asyncio library). The event loop will say
run this coroutine and when it hits the “pause” point the code execution
bubbles back up to the event loop at which point the event loop will
look for other coroutines that have satisfied their awaiting condition.
When it finds a coroutine whose awaitable object has completed it will
give control to that coroutine to execute the code that comes after its
await statement.

This is an oversimplification and you can actually use yield to pause
the execution too, and there are some nuances in terms of executing
certain things to run at the same time, but this is just to get the
basic idea across.

