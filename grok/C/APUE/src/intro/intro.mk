PATHINTRO := src/intro

INTROPROGS := buffered_cat \
              simpleLs \
              tinyShell \
              pidInfo \
              unbuffered_cat

intro: $(addprefix $(PATHINTRO)/,$(INTROPROGS))

$(PATHINTRO)/buffered_cat: $(PATHINTRO)/buffered_cat.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

$(PATHINTRO)/simpleLs: $(PATHINTRO)/simpleLs.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

$(PATHINTRO)/tinyShell: $(PATHINTRO)/tinyShell.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

$(PATHINTRO)/pidInfo: $(PATHINTRO)/pidInfo.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

$(PATHINTRO)/unbuffered_cat: $(PATHINTRO)/unbuffered_cat.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

.PHONY: cleanintro
cleanintro:
	rm -f $(addprefix $(PATHINTRO)/,$(INTROPROGS))
