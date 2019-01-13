ROOT=../..
INCLUDEDIR=$(ROOT)/include
LIBDIR=$(ROOT)/lib
COMMONLIBS=$(ROOT)/commonlibs

CC=gcc
CFLAGS=-Wall -std=c99 -I$(INCLUDEDIR) -L$(LIBDIR)
MAKE=/usr/bin/make

BINARIES=buffered_cat \
         simpleLs \
         tinyShell \
         pidInfo \
         unbuffered_cat

default: spcommonLib binaries

spcommonLib:
	cd $(COMMONLIBS); $(MAKE)

binaries: $(BINARIES)

buffered_cat: buffered_cat.c \
   	          $(INCLUDEDIR)/apue2.h \
              $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -lapue2 

simpleLs: simpleLs.c \
          $(INCLUDEDIR)/apue2.h \
          $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -lapue2 

tinyShell: tinyShell.c \
           $(INCLUDEDIR)/apue2.h \
           $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -lapue2 

pidInfo: pidInfo.c \
         $(INCLUDEDIR)/apue2.h
	$(CC) $(CFLAGS) -o $@ $<

unbuffered_cat: unbuffered_cat.c \
                $(INCLUDEDIR)/apue2.h \
                $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -lapue2 

clean:
	rm -f $(BINARIES)
