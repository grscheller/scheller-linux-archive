CC=gcc
CFLAGS=-Wall -std=gnu11
SUMIT_BINARIES=sumIt sumItO2 sumItV sumItVO2
BINARIES=endianness hw removeSpaces $(SUMIT_BINARIES)

default: binaries

binaries: $(BINARIES)

hw: hw.c
	$(CC) $(CFLAGS) hw.c -o hw

removeSpaces: removeSpaces.c
	$(CC) $(CFLAGS) removeSpaces.c -o removeSpaces

sumIt: sumit.c
	$(CC) sumit.c $(CFLAGS) -o sumIt

sumItO2: sumit.c
	$(CC) sumit.c $(CFLAGS) -O2 -o sumItO2

sumItV: sumit.c
	$(CC) sumit.c $(CFLAGS) -DVOLATILE -o sumItV

sumItVO2: sumit.c
	$(CC) sumit.c $(CFLAGS) -DVOLATILE -O2 -o sumItVO2

timeit: $(SUMIT_BINARIES)
	for bb in $(SUMIT_BINARIES);\
	do\
	    echo -ne "\n$$bb: ";\
	    time ./$$bb;\
	done

clean:
	rm -f $(BINARIES) 
