# Non-recursive, hierarchical makefile to build an APUE project,
# based on third edition of the book
# "Advanced Programming  in the UNIX Environment" by W. Richard Stevens,
# as interpreted by Geoffrey Scheller (geoffrey@scheller.com).

# APUE API and Book Chapters
CHAPTERS := apue intro standards fileio filedir
CLEAN_CHAPTERS := $(addprefix clean,$(CHAPTERS))
INSTALL_CHAPTERS := $(addprefix install,$(CHAPTERS))

all: $(CHAPTERS) $(INSTALL_CHAPTERS)

include config.mk
include src/apue/apue.mk
include src/intro/intro.mk
include src/standards/standards.mk
include src/fileio/fileio.mk
include src/filedir/filedir.mk

clean: $(CLEAN_CHAPTERS)
	rm -f $(INCLUDE)/*.h.gch

.PHONY: all clean $(CHAPTERS) $(INSTALL_CHAPTERS) $(CLEAN_CHAPTERS)
