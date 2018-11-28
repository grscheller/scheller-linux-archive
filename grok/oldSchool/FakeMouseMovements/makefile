CC=gcc
CFLAGS=-Wall
LIBS=-lX11
BINARIES=fakeClick getMouseCoords

default: binaries

binaries: $(BINARIES)

fakeClick: fakeClick.c
	$(CC) $(LIBS) fakeClick.c $(CFLAGS) -o fakeClick

getMouseCoords: getMouseCoords.c
	$(CC) $(LIBS) getMouseCoords.c $(CFLAGS) -o getMouseCoords

clean:
	rm -f fakeClick 
