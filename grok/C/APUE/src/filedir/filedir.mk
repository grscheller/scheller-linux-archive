# Included makefile for chapter 4 - Files and Directories

PATH_FILEDIR := src/filedir
PROGS_FILEDIR := filetype

PROGS_FILEDIR_FULL := $(addprefix $(PATH_FILEDIR)/,$(addsuffix $(EXT),$(PROGS_FILEDIR)))
PROGS_FILEDIR_INST := $(addprefix $(BIN)/,$(addsuffix $(EXT),$(PROGS_FILEDIR)))

filedir: $(PROGS_FILEDIR_FULL)

$(PATH_FILEDIR)/%$(EXT): $(PATH_FILEDIR)/%.c $(APUE_H) $(LIBAPUE_A)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $< $(LDFLAGS) 

installfiledir:
	@cp $(PROGS_FILEDIR_FULL) $(BIN)

cleanfiledir:
	rm -f $(PROGS_FILEDIR_FULL) $(PROGS_FILEDIR_INST)
