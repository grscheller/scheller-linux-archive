ROOT=..
INCLUDEDIR=$(ROOT)/include
LIBDIR=$(ROOT)/lib
COMMONLIBS=$(ROOT)/commonlibs

CC=gcc
CFLAGS=-Wall -std=c99 -I$(INCLUDEDIR) -L$(LIBDIR)
MAKE=/usr/bin/make

BINARIES=cpp_constants \
         seekTest

default: spcommonLib binaries

spcommonLib:
	cd $(COMMONLIBS); $(MAKE)

binaries: $(BINARIES)

cpp_constants: cpp_constants.c \
               $(INCLUDEDIR)/apue2.h \
               $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -lapue2 

seekTest: seekTest.c \
          $(INCLUDEDIR)/apue2.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(BINARIES)
