CC=gcc
CFLAGS=-Wall
HEADERS=../include

O_FILES=spErrorHandlers.o

default: o_files

o_files: $(O_FILES)

spErrorHandlers.o: $(HEADERS)/spHeaders.h \
                   spErrorHandlers.c 
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(O_FILES)
