PATH_INTRO := src/intro
PROGS_INTRO := buffered_cat simpleLs tinyShell pidInfo unbuffered_cat

PROGS_INTRO_FULL = $(addprefix $(PATH_INTRO)/,$(PROGS_INTRO))

intro: $(PROGS_INTRO_FULL)

$(PATH_INTRO)/buffered_cat: $(PATH_INTRO)/buffered_cat.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

$(PATH_INTRO)/simpleLs: $(PATH_INTRO)/simpleLs.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

$(PATH_INTRO)/tinyShell: $(PATH_INTRO)/tinyShell.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

$(PATH_INTRO)/pidInfo: $(PATH_INTRO)/pidInfo.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

$(PATH_INTRO)/unbuffered_cat: $(PATH_INTRO)/unbuffered_cat.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

cleanintro:
	rm -f $(addprefix $(PATH_INTRO)/,$(PROGS_INTRO))
