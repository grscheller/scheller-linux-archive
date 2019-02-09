CC=gcc
CFLAGS=-Wall
BINARIES=hw typedefFu

default: binaries

binaries: $(BINARIES)

hw: hw.c
	$(CC) $(CFLAGS) -o $@ $<

typedefFu: typedefFu.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(BINARIES)
