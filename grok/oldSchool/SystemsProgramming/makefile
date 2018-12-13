CC=gcc
CFLAGS=-Wall -std=c90
BINARIES=buffered_cat \
         simple_ls \
         pidInfo \
         unbuffered_cat

default: binaries

binaries: $(BINARIES)

commonErrorHandlers.o: commonErrorHandlers.c \
                       systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -c commonErrorHandlers.c

buffered_cat: buffered_cat.c \
              commonErrorHandlers.o \
	      systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -o buffered_cat commonErrorHandlers.o buffered_cat.c

unbuffered_cat: unbuffered_cat.c \
                commonErrorHandlers.o \
                systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -o unbuffered_cat commonErrorHandlers.o unbuffered_cat.c

simple_ls: simple_ls.c \
      commonErrorHandlers.o \
      systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -o simple_ls commonErrorHandlers.o simple_ls.c

pidInfo: pidInfo.c \
         systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -o pidInfo pidInfo.c

clean:
	rm -f $(BINARIES) *.o
