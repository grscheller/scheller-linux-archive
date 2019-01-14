ROOT=../..
include $(ROOT)/make.definitions

BINARIES=buffered_cat \
         simpleLs \
         tinyShell \
         pidInfo \
         unbuffered_cat

.PHONY: default libapue2 binaries clean 

default: libapue2 binaries

libapue2:
	cd $(LIBSRC); $(MAKE)

binaries: $(BINARIES)

buffered_cat: buffered_cat.c \
   	          $(INCLUDE)/apue2.h \
              $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -L$(LIBDIR) -lapue2 

simpleLs: simpleLs.c \
          $(INCLUDE)/apue2.h \
          $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -L$(LIBDIR) -lapue2 

tinyShell: tinyShell.c \
           $(INCLUDE)/apue2.h \
           $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -L$(LIBDIR) -lapue2 

pidInfo: pidInfo.c \
         $(INCLUDE)/apue2.h
	$(CC) $(CFLAGS) -o $@ $<

unbuffered_cat: unbuffered_cat.c \
                $(INCLUDE)/apue2.h \
                $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -L$(LIBDIR) -lapue2 

clean:
	rm -f $(BINARIES)
