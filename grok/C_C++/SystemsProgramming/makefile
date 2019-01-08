MAKE=/usr/bin/make

default: spcommonlib chapters

chapters: chap01build chap02build chap03build

clean_chapters: chap01clean chap02clean chap03clean

spcommonlib:
	cd common; \
        $(MAKE)

chap01build:
	cd chap01; \
        $(MAKE) binaries

chap02build:
	cd chap02; \
        $(MAKE) binaries

chap03build:
	cd chap03; \
        $(MAKE) binaries

clean_spcommonlib:
	cd common; \
        $(MAKE) clean

chap01clean:
	cd chap01; \
        $(MAKE) clean

chap02clean:
	cd chap02; \
        $(MAKE) clean

chap03clean:
	cd chap03; \
        $(MAKE) clean

clean: clean_spcommonlib \
       clean_chapters
	rm -f include/*.h.gch
