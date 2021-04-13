# Included makefile for chapter 1 - Introduction

PATH_INTRO := src/intro
PROGS_INTRO := buffered_cat pidInfo simpleLs tinyShell unbuffered_cat

PROGS_INTRO_FULL := $(addprefix $(PATH_INTRO)/,$(addsuffix $(EXT),$(PROGS_INTRO)))
PROGS_INTRO_INST := $(addprefix $(BIN)/,$(addsuffix $(EXT),$(PROGS_INTRO)))

intro: $(PROGS_INTRO_FULL)

$(PATH_INTRO)/%$(EXT): $(PATH_INTRO)/%.c $(APUE_H) $(LIBAPUE_A)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $< $(LDFLAGS)

installintro:
	@cp $(PROGS_INTRO_FULL) $(BIN)

cleanintro:
	rm -f $(PROGS_INTRO_FULL) $(PROGS_INTRO_INST)
