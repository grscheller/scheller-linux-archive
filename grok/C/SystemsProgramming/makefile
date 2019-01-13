MAKE=/usr/bin/make

LIBAPUE2=src/libapue2
CHAPTERS=chap01 chap02 chap03 

default: libapue2 chapters

clean: clean_libapue2 \
       clean_chapters
	rm -f include/*.h.gch

libapue2:
	(cd $(LIBAPUE2) && echo -e "\nmaking $@.a" && $(MAKE)) || exit 1

chapters:
	for ii in $(CHAPTERS); do \
		(cd src/$$ii && echo -e "\nmaking $$ii" && $(MAKE) binaries) || exit 1; \
	done

clean_libapue2:
	(cd $(LIBAPUE2) && echo -e "\ncleaning lib$@.a" && $(MAKE) clean) || exit 1

clean_chapters:
	for ii in $(CHAPTERS); do \
		(cd src/$$ii && echo -e "\ncleaning $$ii" && $(MAKE) clean) || exit 1; \
	done
