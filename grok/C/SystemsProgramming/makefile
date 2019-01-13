MAKE=/usr/bin/make

CHAPTERS=chap01 chap02 chap03 

default: spcommon chapters

clean: clean_spcommon \
       clean_chapters
	rm -f include/*.h.gch

spcommon:
	(cd common && echo -e "\nmaking lib$@.a" && $(MAKE)) || exit 1

chapters:
	for ii in $(CHAPTERS); do \
		(cd $$ii && echo -e "\nmaking $$ii" && $(MAKE) binaries) || exit 1; \
	done

clean_spcommon:
	(cd common && echo -e "\ncleaning lib$@.a" && $(MAKE) clean) || exit 1

clean_chapters:
	for ii in $(CHAPTERS); do \
		(cd $$ii && echo -e "\ncleaning $$ii" && $(MAKE) clean) || exit 1; \
	done
