ROOT=.
include $(ROOT)/make.definitions

CHAPTERS=chap01 chap02 chap03 

.PHONY: default libapue2 chapters clean 

all: libapue2 chapters

libapue2:
	(cd $(LIBSRC) && echo -e "\nmaking $@.a" && $(MAKE)) || exit 1

chapters:
	for ii in $(CHAPTERS); do \
		(cd src/$$ii && echo -e "\nmaking $$ii" && $(MAKE) binaries) || exit 1; \
	done

clean:
	(cd $(LIBSRC) && echo -e "\ncleaning $(LIBSRC)" && $(MAKE) clean) || exit 1
	for ii in $(CHAPTERS); do \
		(cd src/$$ii && echo -e "\ncleaning $$ii" && $(MAKE) clean) || exit 1; \
	done
	rm -f $(INCLUDE)/*.h.gch
