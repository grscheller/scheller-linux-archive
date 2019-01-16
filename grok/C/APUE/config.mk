# APUE Project Configuration
#
# This is NOT a recursive Make build.  The build is
# done from the root APUE directory.

# APUE project defaults and directory stucture 
LIBDIR = lib
INCLUDE = include
SRC = src

# Library and headerfile to support an implementation of
# W. Richard Steven's API for UNIX System Programming.
LIBAPUE_A = $(LIBDIR)/libapue.a
APUE_H = $(INCLUDE)/apue.h

# C compiler configuration
CC = /usr/bin/gcc
CPPFLAGS =
CFLAGS = -Wall -I$(INCLUDE)
LDFLAGS = -L$(LIBDIR) -lapue

# Other UNIX utilities
AR = /usr/bin/ar
AWK = /usr/bin/awk

# Don't use obsolete suffix rules, instead use pattern rules.
.SUFFIXES:
