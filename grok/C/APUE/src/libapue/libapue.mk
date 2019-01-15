# Path to directory from root of build
PATHLIBAPUE := src/libapue
APUE_OBJS := errorHandlers.o limits.o

APUE_OBJS_FULL := $(addprefix $(PATHLIBAPUE)/,$(APUE_OBJS))

$(LIBAPUE): $(APUE_OBJS_FULL)
	[ -d $(LIBDIR) ] || mkdir $(LIBDIR)
	ln $? .
	$(AR) rcsv $(LIBAPUE) $(notdir $?)
	rm $(notdir $?)

$(PATHLIB)/errorHandlers.o: $(PATHLIBAPUE)/errorHandlers.c $(APUE_H)
	$(COMPILE.c) -o $@ $< 

$(PATHLIB)/limits.o: $(PATHLIBAPUE)/limits.c $(APUE_H)
	$(COMPILE.c) -o $@ $< 

cleanlib:
	rm -f $(PATHLIBAPUE)/*.o
	rm -f $(LIBAPUE)
