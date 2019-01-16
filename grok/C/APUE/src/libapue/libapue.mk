# Path to directory from root of build
PATH_APUE := src/libapue
APUE_OBJS := errorHandlers.o limits.o

APUE_OBJS_FULL := $(addprefix $(PATH_APUE)/,$(APUE_OBJS))

$(LIBAPUE): $(APUE_OBJS_FULL)
	[ -d $(LIBDIR) ] || mkdir $(LIBDIR)
	ln $? .
	$(AR) rcsv $(LIBAPUE) $(notdir $?)
	rm $(notdir $?)

$(PATH_APUE)/errorHandlers.o: $(PATH_APUE)/errorHandlers.c $(APUE_H)
	$(COMPILE.c) -o $@ $< 

$(PATH_APUE)/limits.o: $(PATH_APUE)/limits.c $(APUE_H)
	$(COMPILE.c) -o $@ $< 

cleanlib:
	rm -f $(PATH_APUE)/*.o
	rm -f $(LIBAPUE)
