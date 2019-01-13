ROOT=..
INCLUDEDIR=$(ROOT)/include
LIBDIR=$(ROOT)/lib
COMMON=$(ROOT)/common

CC=gcc
CFLAGS=-Wall -std=c99 -I$(INCLUDEDIR) -L$(LIBDIR)
MAKE=/usr/bin/make
AWK=/usr/bin/awk

BINARIES=sysLimits

default: spcommonlib binaries

spcommonlib:
	cd $(COMMON); $(MAKE)

binaries: $(BINARIES)

sysLimits: sysLimits.c \
           $(INCLUDEDIR)/spHeaders.h \
           $(LIBDIR)/libspcommon.a
	$(CC) $(CFLAGS) -o $@ $< -lspcommon

sysLimits.c: genSysLimits.awk \
             sysConf.sym \
             pathConf.sym
	$(AWK) -f genSysLimits.awk > sysLimits.c

clean:
	rm -f $(BINARIES) sysLimits.c
