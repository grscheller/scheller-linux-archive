# Included makefile for chapter 3 - File I/O

PATH_FILEIO := src/fileio
PROGS_FILEIO := cpp_constants stdinSeekable fileSeekable hole

PROGS_FILEIO_FULL := $(addprefix $(PATH_FILEIO)/,$(addsuffix $(EXT),$(PROGS_FILEIO)))

fileio: $(PROGS_FILEIO_FULL)

$(PATH_FILEIO)/%$(EXT): $(PATH_FILEIO)/%.c $(APUE_H) $(LIBAPUE_A)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $< $(LDFLAGS) 

cleanfileio:
	rm -f $(PROGS_FILEIO_FULL)
