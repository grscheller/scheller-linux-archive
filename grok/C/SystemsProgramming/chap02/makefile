ROOT=..
INCLUDEDIR=$(ROOT)/include
LIBDIR=$(ROOT)/lib
COMMON=$(ROOT)/common
COMMONLIBS=$(ROOT)/commonlibs

CC=gcc
CFLAGS=-Wall -std=c99 -I$(INCLUDEDIR) -L$(LIBDIR)
MAKE=/usr/bin/make
AWK=/usr/bin/awk

BINARIES=sysLimits

default: commonLib binaries

commonLib:
	cd $(COMMONLIBS); $(MAKE)

binaries: $(BINARIES)

sysLimits: sysLimits.c \
           $(INCLUDEDIR)/apue2.h \
           $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -lapue2

sysLimits.c: genSysLimits.awk \
             sysConf.sym \
             pathConf.sym
	$(AWK) -f genSysLimits.awk > sysLimits.c

clean:
	rm -f $(BINARIES) sysLimits.c
