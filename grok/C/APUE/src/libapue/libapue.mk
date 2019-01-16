# Included makefile for libapue.a static library

PATH_APUE := src/libapue
OBJS_APUE := errorHandlers.o limits.o

OBJS_APUE_FULL := $(addprefix $(PATH_APUE)/,$(OBJS_APUE))

libapue: $(LIBAPUE_A)

$(LIBAPUE_A): $(OBJS_APUE_FULL)
	[ -d $(LIBDIR) ] || mkdir $(LIBDIR)
	ln $? .
	$(AR) rcsv $(LIBAPUE_A) $(notdir $?)
	rm $(notdir $?)

$(PATH_APUE)/%.o: $(PATH_APUE)/%.c $(APUE_H)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

cleanlibapue:
	rm -f $(PATH_APUE)/*.o
	rm -f $(LIBAPUE_A)
