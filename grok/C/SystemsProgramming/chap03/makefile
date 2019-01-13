ROOT=..
INCLUDEDIR=$(ROOT)/include
LIBDIR=$(ROOT)/lib
COMMON=$(ROOT)/common

CC=gcc
CFLAGS=-Wall -std=c99 -I$(INCLUDEDIR) -L$(LIBDIR)
MAKE=/usr/bin/make

BINARIES=cpp_constants \
         seekTest

default: spcommonlib binaries

spcommonlib:
	cd $(COMMON); $(MAKE)

binaries: $(BINARIES)

cpp_constants: cpp_constants.c \
               $(INCLUDEDIR)/spHeaders.h \
               $(LIBDIR)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon 

seekTest: seekTest.c \
          $(INCLUDEDIR)/spHeaders.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(BINARIES)
