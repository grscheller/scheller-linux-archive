# Path to directory from root of build
PATHLIBAPUE := src/libapue

APUE_OBJS=errorHandlers.o \
          limits.o

$(LIBAPUE): $(addprefix $(PATHLIBAPUE)/,$(APUE_OBJS))
	[ -d $(LIBDIR) ] || mkdir $(LIBDIR)
	ln $? .
	$(AR) rcsv $(LIBAPUE) $(notdir $?)
	rm $(notdir $?)

$(PATHLIB)/errorHandlers.o: $(PATHLIBAPUE)/errorHandlers.c $(APUE_H)
	$(COMPILE.c) -o $@ $< 

$(PATHLIB)/limits.o: $(PATHLIBAPUE)/limits.c $(APUE_H)
	$(COMPILE.c) -o $@ $< 

.PHONY: cleanlib
cleanlib:
	rm -f $(PATHLIBAPUE)/*.o
	rm -f $(LIBAPUE)
