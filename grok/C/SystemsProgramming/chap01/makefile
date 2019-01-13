ROOT=..
INCLUDEDIR=$(ROOT)/include
LIBDIR=$(ROOT)/lib
COMMON=$(ROOT)/common

CC=gcc
CFLAGS=-Wall -std=c99 -I$(INCLUDEDIR) -L$(LIBDIR)
MAKE=/usr/bin/make

BINARIES=buffered_cat \
         simpleLs \
         tinyShell \
         pidInfo \
         unbuffered_cat

default: spcommonlib binaries

spcommonlib:
	cd $(COMMON); $(MAKE)

binaries: $(BINARIES)

buffered_cat: buffered_cat.c \
   	          $(INCLUDEDIR)/spHeaders.h \
              $(LIBDIR)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon 

simpleLs: simpleLs.c \
          $(INCLUDEDIR)/spHeaders.h \
          $(LIBDIR)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon 

tinyShell: tinyShell.c \
           $(INCLUDEDIR)/spHeaders.h \
           $(LIBDIR)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon 

pidInfo: pidInfo.c \
         $(INCLUDEDIR)/spHeaders.h
	$(CC) $(CFLAGS) -o $@ $<

unbuffered_cat: unbuffered_cat.c \
                $(INCLUDEDIR)/spHeaders.h \
                $(LIBDIR)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon 

clean:
	rm -f $(BINARIES)
