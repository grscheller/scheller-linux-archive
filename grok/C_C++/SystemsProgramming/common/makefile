CC=gcc
INCLUDE=../include
CFLAGS=-Wall -std=c99 -I$(INCLUDE)

O_FILES=spErrorHandlers.o \
	spLimits.o

default: o_files

o_files: $(O_FILES)
	

spErrorHandlers.o: spErrorHandlers.c \
                   $(INCLUDE)/spHeaders.h
	$(CC) $(CFLAGS) -c $<

spLimits.o: spLimits.c \
            $(INCLUDE)/spHeaders.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(O_FILES)
