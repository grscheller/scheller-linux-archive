include cc.mk

#TIME=time
TIME=/usr/bin/time -p

SUMIT_BINARIES=sumItBasic sumItO2 sumItV sumItVO2
BINARIES=removeSpaces makeMessage

default: binaries

binaries: $(BINARIES) sumIt

removeSpaces: removeSpaces.c cc.mk
	$(CC) $(CFLAGS) -o $@ $<


sumIt: $(SUMIT_BINARIES)

sumItBasic: sumit.c cc.mk
	$(CC) $(CFLAGS) -o $@ $<

sumItO2: sumit.c cc.mk
	$(CC) $(CFLAGS) -O2 -o $@ $<

sumItV: sumit.c cc.mk
	$(CC) $(CFLAGS) -DVOLATILE -o $@ $<

sumItVO2: sumit.c cc.mk
	$(CC) $(CFLAGS) -DVOLATILE -O2 -o $@ $<

timeIt: $(SUMIT_BINARIES)
	for bb in $(SUMIT_BINARIES); \
    do \
    printf '\n%s: ' "$$bb"; \
        $(TIME) ./$$bb; \
    done


clean:
	rm -f $(BINARIES) $(SUMIT_BINARIES)
