CC=gcc
CFLAGS=-Wall
LIBS=-lX11

default: fakeClick

fakeClick: fakeClick.c
	$(CC) $(LIBS) fakeClick.c $(CFLAGS) -o fakeClick

clean:
	rm -f fakeClick 
