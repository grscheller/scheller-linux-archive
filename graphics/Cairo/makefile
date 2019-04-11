CC=gcc
CFLAGS=-Wall -std=gnu11
BINARIES=hellocairo
OUTPUTS=HelloCairo.svg

default: binaries

binaries: $(BINARIES)

hellocairo: hellocairo.c
	$(CC) $(CFLAGS) -o $@ $< -lcairo

clean:
	rm -f $(BINARIES) $(OUTPUTS)
