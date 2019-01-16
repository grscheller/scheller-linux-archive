# Included makefile for chapter 2 - Unix Standards and Implementations

PATH_FILEIO := src/fileio
PROGS_FILEIO := cpp_constants seekTest

PROGS_FILEIO_FULL := $(addprefix $(PATH_FILEIO)/,$(PROGS_FILEIO))

fileio: $(PROGS_FILEIO_FULL)

$(PATH_FILEIO)/cpp_constants: $(PATH_FILEIO)/cpp_constants.c $(APUE_H) $(LIBAPUE_A)

$(PATH_FILEIO)/seekTest: $(PATH_FILEIO)/seekTest.c $(APUE_H) $(LIBAPUE_A)

cleanfileio:
	rm -f $(PROGS_FILEIO_FULL)
