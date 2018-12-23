CC=gcc
CFLAGS=-Wall
BINARIES=buffered_cat \
         simple_ls \
         tinyShell \
         pidInfo \
         unbuffered_cat

default: binaries

binaries: $(BINARIES)

commonErrorHandlers.o: commonErrorHandlers.c \
                       systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -c $<

buffered_cat: buffered_cat.c \
              commonErrorHandlers.o \
	      systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -o $@ commonErrorHandlers.o $<

simple_ls: simple_ls.c \
      commonErrorHandlers.o \
      systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -o $@ commonErrorHandlers.o $<

tinyShell: tinyShell.c \
      commonErrorHandlers.o \
      systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -o $@ commonErrorHandlers.o $<

pidInfo: pidInfo.c \
         systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -o $@ $<

unbuffered_cat: unbuffered_cat.c \
                commonErrorHandlers.o \
                systemsProgrammingHeaders.h
	$(CC) $(CFLAGS) -o $@ commonErrorHandlers.o $<

clean:
	rm -f $(BINARIES) *.o
