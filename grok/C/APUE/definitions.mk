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

# Steven's API for UNIX System Programming
LIBAPUE = $(LIBDIR)/libapue.a
LDAPUE = -lapue
LIBAPUESRC = src/libapue
APUE_H = $(INCLUDE)/apue.h

# Other UNIX utilities
AR = /usr/bin/ar
AWK = /usr/bin/awk
