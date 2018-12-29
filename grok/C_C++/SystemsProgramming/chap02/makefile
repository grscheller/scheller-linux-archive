CC=gcc
INCLUDE=../include
CFLAGS=-Wall -I$(INCLUDE)
MAKE=/usr/bin/make

BINARIES=sysLimits

COMMON=../common

default: object_files binaries

object_files:
	cd $(COMMON); \
        $(MAKE)

binaries: $(BINARIES)

sysLimits: sysLimits.c \
	   $(INCLUDE)/spHeaders.h \
           $(COMMON)/spErrorHandlers.o
	$(CC) $(CFLAGS) -o $@ $(COMMON)/spErrorHandlers.o $<

sysLimits.c: genSysLimits.awk \
             sysConf.sym \
	     pathConf.sym
	./genSysLimits.awk > sysLimits.c

clean:
	rm -f $(BINARIES) sysLimits.c
