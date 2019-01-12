CC=gcc
INCLUDE=../include
LIB=../lib
CFLAGS=-Wall -std=c99 -I$(INCLUDE)
AR=/usr/bin/ar

O_FILES=spErrorHandlers.o \
        spLimits.o

default: $(LIB)/libspcommon.a

../lib/libspcommon.a: $(O_FILES)
	$(AR) rcs $(LIB)/libspcommon.a $?

spErrorHandlers.o: spErrorHandlers.c \
                   $(INCLUDE)/spHeaders.h
	$(CC) $(CFLAGS) -c $<

spLimits.o: spLimits.c \
            $(INCLUDE)/spHeaders.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(O_FILES)
	rm -f $(LIB)/libspcommon.a
