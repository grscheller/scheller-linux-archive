# Non-recursive, hierarchical makefile to build an APUE project.
# Based on the third edition of the W. Richard Stevens' book
# "Advanced Programming  in the UNIX Environment" as interpreted
# by Geoffrey Scheller (geoffrey@scheller.com).

# System specific and build configurations
include config.mk

# APUE Book Chapters
CHAPTERS := intro standards fileio 
CLEAN_CHAPTERS := $(addprefix clean,$(CHAPTERS))
INSTALL_CHAPTERS := $(addprefix install,$(CHAPTERS))

# Implementation of APUE Systems Programming API
APUE := apue
CLEAN_APUE := clean$(APUE)

all: $(APUE) $(CHAPTERS) $(INSTALL_CHAPTERS)

include src/apue/apue.mk
include src/intro/intro.mk
include src/standards/standards.mk
include src/fileio/fileio.mk

clean: $(CLEAN_CHAPTERS) $(CLEAN_APUE)
	rm -f $(INCLUDE)/*.h.gch

.PHONY: all $(APUE) $(CHAPTERS) $(INSTALL_CHAPTERS)
.PHONY: clean $(CLEAN_APUE) $(CLEAN_CHAPTERS)
