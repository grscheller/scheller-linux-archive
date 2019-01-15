# Non-recursive, hierarchical makefile to build an APUE project.
# Based on the third edition of the W. Richard Stevens' book
# "Advanced Programming  in the UNIX Environment" as interpreted
# by Geoffrey Scheller (geoffrey@scheller.com).

# System specific and build configurations
include definitions.mk

# APUE Book Chapters
#CHAPTERS := intro standards fileio 
CHAPTERS := intro

# Default source code directory
SRC = src

.PHONY: all $(CHAPTERS) clean 

all: $(LIBAPUE) $(CHAPTERS)

include $(LIBAPUESRC)/libapue.mk
include $(SRC)/intro/intro.mk
# include $(SRC)/standards/make.standards
# include $(SRC)/fileio/make.fileio

clean: cleanlib cleanintro
	rm -f $(INCLUDE)/*.h.gch
