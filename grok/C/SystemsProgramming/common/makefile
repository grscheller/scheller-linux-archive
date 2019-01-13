ROOT=..
INCLUDEDIR=$(ROOT)/include
LIBDIR=$(ROOT)/lib

CC=gcc
CFLAGS=-Wall -std=c99 -I$(INCLUDEDIR)
AR=/usr/bin/ar

OBJS=spErrorHandlers.o \
     spLimits.o

default: $(LIBDIR)/libspcommon.a

../lib/libspcommon.a: $(OBJS)
	$(AR) rcs $(LIBDIR)/libspcommon.a $?

spErrorHandlers.o: spErrorHandlers.c \
                   $(INCLUDEDIR)/spHeaders.h
	$(CC) $(CFLAGS) -c $<

spLimits.o: spLimits.c \
            $(INCLUDEDIR)/spHeaders.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(OBJS)
	rm -f $(LIBDIR)/libspcommon.a
