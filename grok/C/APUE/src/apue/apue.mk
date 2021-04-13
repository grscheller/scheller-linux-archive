# Included makefile for libapue.a static library

PATH_APUE := src/apue
OBJS_APUE := error.o errorlog.o limits.o make_message.o

OBJS_APUE_FULL := $(addprefix $(PATH_APUE)/,$(OBJS_APUE))

apue: $(APUE_H) $(LIBAPUE_A)

$(APUE_H): $(PATH_APUE)/apue.h
	cp $< $@

$(LIBAPUE_A): $(OBJS_APUE_FULL)
	[ -d $(LIBDIR) ] || mkdir $(LIBDIR)
	ln $? .
	$(AR) rcsv $(LIBAPUE_A) $(notdir $?)
	rm $(notdir $?)

$(PATH_APUE)/%.o: $(PATH_APUE)/%.c $(APUE_H)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

cleanapue:
	rm -f $(PATH_APUE)/*.o
	rm -f $(LIBAPUE_A)
	rm -f $(APUE_H)
