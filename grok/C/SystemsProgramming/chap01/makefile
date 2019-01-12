CC=gcc
INCLUDE=../include
LIB=../lib
COMMON=../common
CFLAGS=-Wall -std=c99 -I$(INCLUDE) -L$(LIB)
MAKE=/usr/bin/make

BINARIES=buffered_cat \
         simpleLs \
         tinyShell \
         pidInfo \
         unbuffered_cat

default: spcommonlib binaries

spcommonlib:
	cd $(COMMON); \
        $(MAKE)

binaries: $(BINARIES)

buffered_cat: buffered_cat.c \
	      $(INCLUDE)/spHeaders.h \
              $(LIB)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon 

simpleLs: simpleLs.c \
           $(INCLUDE)/spHeaders.h \
           $(LIB)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon 

tinyShell: tinyShell.c \
           $(INCLUDE)/spHeaders.h \
           $(LIB)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon 

pidInfo: pidInfo.c \
         $(INCLUDE)/spHeaders.h
	$(CC) $(CFLAGS) -o $@ $<

unbuffered_cat: unbuffered_cat.c \
                $(INCLUDE)/spHeaders.h \
                $(LIB)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon 

clean:
	rm -f $(BINARIES)
