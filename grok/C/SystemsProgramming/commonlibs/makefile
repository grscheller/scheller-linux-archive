ROOT=..
INCLUDEDIR=$(ROOT)/include
LIBDIR=$(ROOT)/lib

CC=gcc
CFLAGS=-Wall -std=c99 -I$(INCLUDEDIR)
AR=/usr/bin/ar

OBJS=errorHandlers.o \
     limits.o

default: $(LIBDIR)/libapue2.a

../lib/libapue2.a: $(OBJS)
	$(AR) rcs $(LIBDIR)/libapue2.a $?

errorHandlers.o: errorHandlers.c \
                 $(INCLUDEDIR)/apue2.h
	$(CC) $(CFLAGS) -c $<

limits.o: limits.c \
          $(INCLUDEDIR)/apue2.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(OBJS)
	rm -f $(LIBDIR)/libapue2.a
