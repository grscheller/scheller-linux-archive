CC=gcc
#CC=clang
CFLAGS=-Wall
SUMIT_BINARIES=sumItBasic sumItO2 sumItV sumItVO2
BINARIES=removeSpaces makeMessage

default: binaries

binaries: $(BINARIES) sumIt

removeSpaces: removeSpaces.c
	$(CC) $(CFLAGS) -o $@ $<


sumIt: $(SUMIT_BINARIES)

sumItBasic: sumit.c
	$(CC) $(CFLAGS) -o $@ $<

sumItO2: sumit.c
	$(CC) $(CFLAGS) -O2 -o $@ $<

sumItV: sumit.c
	$(CC) $(CFLAGS) -DVOLATILE -o $@ $<

sumItVO2: sumit.c
	$(CC) $(CFLAGS) -DVOLATILE -O2 -o $@ $<

timeIt: $(SUMIT_BINARIES)
	for bb in $(SUMIT_BINARIES);\
    do\
        echo -ne "\n$$bb: ";\
        time ./$$bb;\
    done

clean:
	rm -f $(BINARIES) $(SUMIT_BINARIES)
