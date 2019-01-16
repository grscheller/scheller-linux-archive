# Non-recursive, hierarchical makefile to build an APUE project.
# Based on the third edition of the W. Richard Stevens' book
# "Advanced Programming  in the UNIX Environment" as interpreted
# by Geoffrey Scheller (geoffrey@scheller.com).

# System specific and build configurations
include config.mk

# APUE Book Chapters
CHAPTERS := intro standards fileio 
CLEANCHAPTERS := $(addprefix clean,$(CHAPTERS))

# all: $(LIBAPUE) $(CHAPTERS)
all: $(CHAPTERS)

include src/libapue/libapue.mk
include src/intro/intro.mk
include src/standards/standards.mk
include src/fileio/fileio.mk

clean: cleanlibapue $(CLEANCHAPTERS)
	rm -f $(INCLUDE)/*.h.gch

.PHONY: all libapue $(CHAPTERS)
.PHONY: clean cleanlibapue $(CLEANCHAPTERS)
