PATH_FILEIO := src/fileio
PROGS_FILEIO := cpp_constants seekTest

PROGS_FILEIO_FULL := $(addprefix $(PATH_FILEIO)/,$(PROGS_FILEIO))

fileio: $(PROGS_FILEIO_FULL)

$(PATH_FILEIO)/cpp_constants: $(PATH_FILEIO)/cpp_constants.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

$(PATH_FILEIO)/seekTest: $(PATH_FILEIO)/seekTest.c $(APUE_H) $(LIBAPUE)
	$(LINK.c) -o $@ $< $(LDLIBS)

cleanfileio:
	rm -f $(PROGS_FILEIO_FULL)
