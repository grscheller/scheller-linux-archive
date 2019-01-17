# APUE Project Configuration
#
# This is NOT a recursive Make build.  The build is
# done from the root APUE directory.

# APUE project defaults and directory stucture 
LIBDIR = lib
INCLUDE = include

# Library and headerfile to support an implementation of
# W. Richard Steven's API for UNIX System Programming.
LIBAPUE_A = $(LIBDIR)/libapue.a
APUE_H = $(INCLUDE)/apue.h

# Compiler flags for specific OS's
# All feature macros defined in apue.h
LINUX_CFLAGS = -std=c99 -DLINUX
FREEBSD_CFLAGS = -ansi -DBSD                     # untested
MACOS_CFLAGS = -ansi -DMACOS                     # untested
SOLARIS_CFLAGS = -std=c99 -m64 -DSOLARIS         # untested

# Select for your system
SYSTEM_CFLAGS := $(LINUX_CFLAGS)

# C compiler configuration
CC = gcc
CPPFLAGS =
CFLAGS = $(SYSTEM_CFLAGS) -Wall -I$(INCLUDE)
LDFLAGS = -L$(LIBDIR) -lapue

# Other UNIX utilities
AR = ar
AWK = awk

# Don't use obsolete suffix rules, instead use pattern rules.
.SUFFIXES:
