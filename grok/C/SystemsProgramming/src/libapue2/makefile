ROOT=../..
include $(ROOT)/make.definitions

OBJS=errorHandlers.o \
     limits.o

default: $(LIBAPUE2)

../../lib/libapue2.a: $(OBJS)
	$(AR) rcs $(LIBAPUE2) $?

errorHandlers.o: errorHandlers.c \
                 $(INCLUDE)/apue2.h
	$(CC) $(CFLAGS) -c $<

limits.o: limits.c \
          $(INCLUDE)/apue2.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(OBJS)
	rm -f $(LIBAPUE2)
