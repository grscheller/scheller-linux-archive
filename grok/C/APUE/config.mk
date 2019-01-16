# APUE Project Configuration
#
# This is NOT a recursive Make build.  The build is
# done from the root APUE directory.

# APUE project defaults and directory stucture 
LIBDIR := lib
INCLUDE := include
CFLAGS_DEFAULT := -Wall -I$(INCLUDE)
LDFLAGS_DEFAULT := -L$(LIBDIR)

# Support an implementation of W. Richard Steven's API
# for UNIX System Programminga.
LIBAPUE_A := $(LIBDIR)/libapue.a
APUE_H := $(INCLUDE)/apue.h
LDFLAGS_APUE := -lapue

# C compiler configuration
CC = /usr/bin/gcc
CFLAGS = $(CFLAG_DEFAULT) $(CFLAGS_EXTRA)
LDFLAGS = $(LDFLAGS_DEFAULT) $(LDFLAGS_APUE) $(LDFLAGS_EXTRA)
COMPILE.c = $(CC) $(CFLAGS) $(CPPFLAGS) -c
LINK.c = $(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS)

# Other UNIX utilities
AR = /usr/bin/ar
AWK = /usr/bin/awk
