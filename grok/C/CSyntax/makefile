CC=gcc
CFLAGS=-Wall
BINARIES=concatAdjStrings hw typedefFu

default: binaries

binaries: $(BINARIES)

concatAdjStrings: concatAdjStrings.c
	$(CC) $(CFLAGS) -o $@ $<

hw: hw.c
	$(CC) $(CFLAGS) -o $@ $<

typedefFu: typedefFu.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(BINARIES)
