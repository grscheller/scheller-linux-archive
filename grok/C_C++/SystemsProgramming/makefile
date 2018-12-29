MAKE=/usr/bin/make

default: obj_files chapters

chapters: chap01build

clean_chapters: chap01clean

obj_files:
	cd common; \
        $(MAKE)

chap01build:
	cd chap01; \
        $(MAKE) binaries

clean_obj_files:
	cd common; \
        $(MAKE) clean

chap01clean:
	cd chap01; \
        $(MAKE) clean

clean: clean_obj_files \
       clean_chapters
	rm -f include/*.h.gch
