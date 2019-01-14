ROOT=../..
include $(ROOT)/make.definitions

BINARIES=sysLimits

.PHONY: default libapue2 binaries clean 

default: libapue2 binaries

libapue2:
	cd $(LIBSRC); $(MAKE)

binaries: $(BINARIES)

sysLimits: sysLimits.c \
           $(INCLUDE)/apue2.h \
           $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -L$(LIBDIR) -lapue2

sysLimits.c: genSysLimits.awk \
             sysConf.sym \
             pathConf.sym
	$(AWK) -f genSysLimits.awk > sysLimits.c

clean:
	rm -f $(BINARIES) sysLimits.c
