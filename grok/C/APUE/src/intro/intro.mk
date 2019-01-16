# Included makefile for chapter 1 - Introduction

PATH_INTRO := src/intro
PROGS_INTRO := buffered_cat pidInfo simpleLs tinyShell unbuffered_cat

PROGS_INTRO_FULL := $(addprefix $(PATH_INTRO)/,$(PROGS_INTRO))

intro: $(PROGS_INTRO_FULL)

$(PATH_INTRO)/buffered_cat: $(PATH_INTRO)/buffered_cat.c $(APUE_H) $(LIBAPUE_A)

$(PATH_INTRO)/simpleLs: $(PATH_INTRO)/simpleLs.c $(APUE_H) $(LIBAPUE_A)

$(PATH_INTRO)/tinyShell: $(PATH_INTRO)/tinyShell.c $(APUE_H) $(LIBAPUE_A)

$(PATH_INTRO)/pidInfo: $(PATH_INTRO)/pidInfo.c $(APUE_H) $(LIBAPUE_A)

$(PATH_INTRO)/unbuffered_cat: $(PATH_INTRO)/unbuffered_cat.c $(APUE_H) $(LIBAPUE_A)

cleanintro:
	rm -f $(PROGS_INTRO_FULL)
