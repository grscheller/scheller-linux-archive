CC=gcc
CFLAGS=-Wall -std=gnu11
BINARIES=fakeClick getMouseCoords jiggleMouse

default: binaries

binaries: $(BINARIES)

fakeClick: fakeClick.c
	$(CC) $(CFLAGS) -o $@ $< -lX11

jiggleMouse: jiggleMouse.c
	$(CC) $(CFLAGS) -o $@ $< -lX11

clean:
	rm -f $(BINARIES)
