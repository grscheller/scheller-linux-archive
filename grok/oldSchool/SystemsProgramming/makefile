CC=gcc
CFLAGS=-Wall -std=c90
BINARIES=myLs myUnbufferedCat myBufferedCat pidInfo

default: binaries

binaries: $(BINARIES)

myErrorHandlers.o: myErrorHandlers.c \
                   myCommon.h

myBufferedCat: myBufferedCat.c \
               myErrorHandlers.o \
	       myCommon.h
	$(CC) $(CFLAGS) -o myBufferedCat myErrorHandlers.o myBufferedCat.c

myUnbufferedCat: myUnbufferedCat.c \
                 myErrorHandlers.o \
		 myCommon.h
	$(CC) $(CFLAGS) -o myUnbufferedCat myErrorHandlers.o myUnbufferedCat.c

myLs: myLs.c \
      myErrorHandlers.o \
      myCommon.h
	$(CC) $(CFLAGS) -o myLs myErrorHandlers.o myLs.c

pidInfo: pidInfo.c
	$(CC) $(CFLAGS) -o pidInfo pidInfo.c

clean:
	rm -f $(BINARIES) *.o
