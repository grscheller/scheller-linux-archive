# File containing system and compiler specific options.
# Also, select implementaion of APUE API.

# C compiler configuration
CC = /usr/bin/gcc
COMPILE.c = $(CC) $(CFLAGS) $(CPPFLAGS) -c
LINK.o = $(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS)
INCLUDES = $(PRE_INCLUDES) -I$(INCLUDE) $(POST_INCLUDES)
CFLAGS = -Wall $(INCLUDES) $(CEXTRAFLAGS)
LDFLAGS = $(LDEXTRAFLAGS)
LDDIRS = $(PRE_LDDIRS) -L$(LIBDIR) $(POST_LDDIRS)
LDLIBS = $(LDDIRS) $(PRE_LDLIBS) $(LDAPUE) $(POST_LDLIBS)
INCLUDE = include
LIBDIR = lib

# Other UNIX utilities
SHELL = /bin/sh
AR = /usr/bin/ar
AWK = /usr/bin/awk

# Steven's API for UNIX System Programming
LIBAPUE = $(LIBDIR)/libapue2.a
LDAPUE = -lapue2
LIBAPUESRC = src/libapue2
APUE_H = $(INCLUDE)/apue2.h

# Lets be explicit about implicit rules
.SUFFIXES:
.SUFFIXES: .c .o
# .SUFFIXES: .y .c

%:	%.o $(LIBAPUE)
	$(LINK.o) $@.o -o $@ $(LDLIBS)
