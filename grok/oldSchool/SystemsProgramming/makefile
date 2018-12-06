CC=gcc
CFLAGS=-Wall
BINARIES=myls

default: binaries

binaries: $(BINARIES)

myls: myls.c myErrorHandlers.o myErrorHandlers.h
	$(CC) $(CFLAGS) -o myls myErrorHandlers.o myls.c

clean:
	rm -f $(BINARIES) *.o
