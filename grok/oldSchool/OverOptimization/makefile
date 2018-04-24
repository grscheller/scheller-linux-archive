CC=gcc
CFLAGS=-Wall -std=c99
BINARIES=sumIt sumItO2 sumItV sumItVO2

default: binaries

binaries: $(BINARIES)

sumIt: main.c
	$(CC) main.c $(CFLAGS) -o sumIt

sumItO2: main.c
	$(CC) main.c $(CFLAGS) -O2 -o sumItO2

sumItV: main.c
	$(CC) main.c $(CFLAGS) -DVOLATILE -o sumItV

sumItVO2: main.c
	$(CC) main.c $(CFLAGS) -DVOLATILE -O2 -o sumItVO2

timeit: $(BINARIES)
	for bb in $(BINARIES);\
	do\
	    echo -ne "\n$$bb: ";\
	    time $$bb;\
	done

clean:
	rm -f $(BINARIES) 
