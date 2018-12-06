CC=gcc
CFLAGS=-Wall
BINARIES=hw removeSpaces

default: binaries

binaries: $(BINARIES)

hw: hw.c
	$(CC) $(CFLAGS) hw.c -o hw

removeSpaces: removeSpaces.c
	$(CC) $(CFLAGS) removeSpaces.c -o removeSpaces

clean:
	rm -f $(BINARIES)
