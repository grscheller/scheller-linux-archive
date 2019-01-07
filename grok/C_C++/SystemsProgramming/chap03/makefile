CC=gcc
INCLUDE=../include
LIB=../lib
COMMON=../common
CFLAGS=-Wall -std=c99 -I$(INCLUDE) -L$(LIB)
MAKE=/usr/bin/make

BINARIES=cpp_constants \

default: spcommonlib binaries

spcommonlib:
	cd $(COMMON); \
        $(MAKE)

binaries: $(BINARIES)

cpp_constants: cpp_constants.c \
	      $(INCLUDE)/spHeaders.h
	$(CC) $(CFLAGS) -o $@ $< -lspcommon 

clean:
	rm -f $(BINARIES)
