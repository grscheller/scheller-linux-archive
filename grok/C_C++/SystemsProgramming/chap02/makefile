CC=gcc
INCLUDE=../include
LIB=../lib
COMMON=../common
CFLAGS=-Wall -std=c99 -I$(INCLUDE) -L$(LIB)
MAKE=/usr/bin/make

BINARIES=sysLimits

default: spcommonlib binaries

spcommonlib:
	cd $(COMMON); \
        $(MAKE)

binaries: $(BINARIES)

sysLimits: sysLimits.c \
	   $(INCLUDE)/spHeaders.h \
           $(LIB)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon

sysLimits.c: genSysLimits.awk \
             sysConf.sym \
	     pathConf.sym
	./genSysLimits.awk > sysLimits.c

clean:
	rm -f $(BINARIES) sysLimits.c
