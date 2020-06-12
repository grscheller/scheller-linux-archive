CC=gcc
CFLAGS=-Wall
FTM_BINARIES=ftmBasic \
             ftmXOpen500 ftmXOpen600 ftmXOpen700 ftmXOpen \
             ftmISOC99 ftmISOC11 \
             ftmPOSIX ftmPOSIX1 ftmPOSIX2 \
             ftmPOSIX199309L ftmPOSIX199506L ftmPOSIX200112L \
             ftmPOSIX200809L ftmPOSIX202012L \
             ftmGNU
BINARIES=endianness gnuPosixComplianceTrick korn

default: binaries

binaries: $(BINARIES) ftm

endianness: endianness.c
	$(CC) $(CFLAGS) -o $@ $<

gnuPosixComplianceTrick: gnuPosixComplianceTrick.c gnuPosixComplianceTrick.h
	$(CC) $(CFLAGS) -o $@ $<

korn: korn.c
	$(CC) $(CFLAGS) -o $@ $<

ftm: $(FTM_BINARIES)

ftmBasic: featureTestMacros.c
	$(CC) $(CFLAGS) -o ftmBasic $<

ftmXOpen500: featureTestMacros.c
	$(CC) $(CFLAGS) -D_XOPEN_SOURCE=500 -o $@ $<

ftmXOpen600: featureTestMacros.c
	$(CC) $(CFLAGS) -D_XOPEN_SOURCE=600 -o $@ $<

ftmXOpen700: featureTestMacros.c
	$(CC) $(CFLAGS) -D_XOPEN_SOURCE=700 -o $@ $<

ftmXOpen: featureTestMacros.c
	$(CC) $(CFLAGS) -D_XOPEN_SOURCE -o $@ $<

ftmISOC99: featureTestMacros.c
	$(CC) $(CFLAGS) -D_IOSC99 -o $@ $<

ftmISOC11: featureTestMacros.c
	$(CC) $(CFLAGS) -D_IOSC11_SOURCE -o $@ $<

ftmPOSIX: featureTestMacros.c
	$(CC) $(CFLAGS) -D_POSIX_C_SOURCE -o $@ $<

ftmPOSIX1: featureTestMacros.c
	$(CC) $(CFLAGS) -D_POSIX_C_SOURCE=1 -o $@ $<

ftmPOSIX2: featureTestMacros.c
	$(CC) $(CFLAGS) -D_POSIX_C_SOURCE=2 -o $@ $<

ftmPOSIX199309L: featureTestMacros.c
	$(CC) $(CFLAGS) -D_POSIX_C_SOURCE=199309L -o $@ $<

ftmPOSIX199506L: featureTestMacros.c
	$(CC) $(CFLAGS) -D_POSIX_C_SOURCE=199506L -o $@ $<

ftmPOSIX200112L: featureTestMacros.c
	$(CC) $(CFLAGS) -D_POSIX_C_SOURCE=200112L -o $@ $<

ftmPOSIX200809L: featureTestMacros.c
	$(CC) $(CFLAGS) -D_POSIX_C_SOURCE=200809L -o $@ $<

ftmPOSIX202012L: featureTestMacros.c
	$(CC) $(CFLAGS) -D_POSIX_C_SOURCE=202012L -o $@ $<

ftmGNU: featureTestMacros.c
	$(CC) $(CFLAGS) -D_GNU_SOURCE -o $@ $<

testSys: $(FTM_BINARIES)
	for bb in $(FTM_BINARIES);\
	do\
	    echo -ne "\n$$bb:\n";\
	    ./$$bb;\
	done

clean:
	rm -f $(BINARIES) $(FTM_BINARIES)
