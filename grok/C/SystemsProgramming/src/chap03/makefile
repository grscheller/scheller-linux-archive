ROOT=../..
include $(ROOT)/make.definitions

BINARIES=cpp_constants \
         seekTest

.PHONY: default libapue2 binaries clean 

default: libapue2 binaries

libapue2:
	cd $(LIBSRC); $(MAKE)

binaries: $(BINARIES)

cpp_constants: cpp_constants.c \
               $(INCLUDE)/apue2.h \
               $(LIBDIR)/libapue2.a
	$(CC) $(CFLAGS) -o $@ $< -L$(LIBDIR) -lapue2 

seekTest: seekTest.c \
          $(INCLUDE)/apue2.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(BINARIES)
