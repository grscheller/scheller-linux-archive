CC=gcc
CFLAGS=-Wall
MAKE=/usr/bin/make
HEADERS=../include

BINARIES=buffered_cat \
         simple_ls \
         tinyShell \
         pidInfo \
         unbuffered_cat

COMMON=../common

default: object_files binaries

object_files:
	cd $(COMMON); \
        $(MAKE)

binaries: $(BINARIES)

buffered_cat: buffered_cat.c \
	      $(HEADERS)/spHeaders.h \
              $(COMMON)/spErrorHandlers.o
	$(CC) $(CFLAGS) -o $@ $(COMMON)/spErrorHandlers.o $<

simple_ls: simple_ls.c \
           $(HEADERS)/spHeaders.h \
           $(COMMON)/spErrorHandlers.o
	$(CC) $(CFLAGS) -o $@ $(COMMON)/spErrorHandlers.o $<

tinyShell: tinyShell.c \
           $(HEADERS)/spHeaders.h \
           $(COMMON)/spErrorHandlers.o
	$(CC) $(CFLAGS) -o $@ $(COMMON)/spErrorHandlers.o $<

pidInfo: pidInfo.c \
         $(HEADERS)/spHeaders.h
	$(CC) $(CFLAGS) -o $@ $<

unbuffered_cat: unbuffered_cat.c \
                $(HEADERS)/spHeaders.h \
                $(COMMON)/spErrorHandlers.o
	$(CC) $(CFLAGS) -o $@ $(COMMON)/spErrorHandlers.o $<

clean:
	rm -f $(BINARIES)
