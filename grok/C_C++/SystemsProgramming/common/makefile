CC=gcc
INCLUDE=../include
CFLAGS=-Wall -I$(INCLUDE)

O_FILES=spErrorHandlers.o

default: o_files

o_files: $(O_FILES)
	

spErrorHandlers.o: spErrorHandlers.c \
                   $(INCLUDE)/spHeaders.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(O_FILES)
