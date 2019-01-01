CC=gcc
INCLUDE=../include
CFLAGS=-Wall -std=c99 -I$(INCLUDE)
MAKE=/usr/bin/make

BINARIES=buffered_cat \
         simpleLs \
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
	      $(INCLUDE)/spHeaders.h \
              $(COMMON)/spErrorHandlers.o
	$(CC) $(CFLAGS) -o $@ $(COMMON)/spErrorHandlers.o $<

simpleLs: simpleLs.c \
           $(INCLUDE)/spHeaders.h \
           $(COMMON)/spErrorHandlers.o
	$(CC) $(CFLAGS) -o $@ $(COMMON)/spErrorHandlers.o $<

tinyShell: tinyShell.c \
           $(INCLUDE)/spHeaders.h \
           $(COMMON)/spErrorHandlers.o
	$(CC) $(CFLAGS) -o $@ $(COMMON)/spErrorHandlers.o $<

pidInfo: pidInfo.c \
         $(INCLUDE)/spHeaders.h
	$(CC) $(CFLAGS) -o $@ $<

unbuffered_cat: unbuffered_cat.c \
                $(INCLUDE)/spHeaders.h \
                $(COMMON)/spErrorHandlers.o
	$(CC) $(CFLAGS) -o $@ $(COMMON)/spErrorHandlers.o $<

clean:
	rm -f $(BINARIES)
