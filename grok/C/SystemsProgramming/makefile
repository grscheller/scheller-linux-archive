MAKE=/usr/bin/make

CHAPTERS=chap01 chap02 chap03 

default: commonLibs chapters

clean: clean_commonLibs \
       clean_chapters
	rm -f include/*.h.gch

commonLibs:
	(cd commonlibs && echo -e "\nmaking lib$@.a" && $(MAKE)) || exit 1

chapters:
	for ii in $(CHAPTERS); do \
		(cd $$ii && echo -e "\nmaking $$ii" && $(MAKE) binaries) || exit 1; \
	done

clean_commonLibs:
	(cd commonlibs && echo -e "\ncleaning lib$@.a" && $(MAKE) clean) || exit 1

clean_chapters:
	for ii in $(CHAPTERS); do \
		(cd $$ii && echo -e "\ncleaning $$ii" && $(MAKE) clean) || exit 1; \
	done
