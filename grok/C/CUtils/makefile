#CC=cc
#CC=gcc
CC=clang
CFLAGS=-Wall
UTILS=realpath
XUTILS=jigglemouseX

default: binaries

binaries: $(UTILS) $(XUTILS)

jigglemouseX: jigglemouseX.c
	$(CC) $(CFLAGS) -o $@ $< -lX11

realpath: realpath.c
	$(CC) $(CFLAGS) -o $@ $<

install: binaries
	mv $(UTILS) $(XUTILS) ~/.local/bin

clean:
	rm -f $(UTILS) $(XUTILS)

