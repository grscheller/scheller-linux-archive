# Non-recursive, hierarchical makefile to build an APUE project.
# Based on the third edition of the W. Richard Stevens' book
# "Advanced Programming  in the UNIX Environment" as interpreted
# by Geoffrey Scheller (geoffrey@scheller.com).

# System specific and build configurations
include config.mk

# APUE Book Chapters
CHAPTERS := intro standards fileio 
CLEAN_CHAPTERS := $(addprefix clean,$(CHAPTERS))

# Implementation of W. Richard Stevens' "Advanced Programming
# in the UNIX Environment" Systems Programming API
APUE := libapue
CLEAN_APUE := clean$(APUE)

all: $(CHAPTERS) $(APUE)

include src/libapue/libapue.mk
include src/intro/intro.mk
include src/standards/standards.mk
include src/fileio/fileio.mk

clean: $(CLEAN_CHAPTERS) $(CLEAN_APUE)
	rm -f $(INCLUDE)/*.h.gch

.PHONY: all $(CHAPTERS) $(APUE)
.PHONY: clean $(CLEAN_CHAPTERS) $(CLEAN_APUE)
