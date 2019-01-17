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

# Compiler flags for specific OS's and/or compiler combinations
LINUX_GCC_FLAGS = -ansi -DLINUX -D_GNU_SOURCE
FREEBSD_FLAGS = -ansi -DBSD -D__BSD_VISIBLE
MACOS_FLAGS = -ansi -DMACOS -D_DARWIN_C_SOURCE
SOLARIS_FLAGS = -std=c99 -m64 -DSOLARIS -D__EXTENSIONS__
LINUX_GCC_GEOFFREY_FLAGS = -DLINUX -D_GNU_SOURCE -O3

# Select for your system
SYSTEM_FLAGS = $(LINUX_GCC_FLAGS)

# C compiler configuration
CC = /usr/bin/gcc
CPPFLAGS =
CFLAGS = $(SYSTEM_FLAGS) -Wall -I$(INCLUDE)
LDFLAGS = -L$(LIBDIR) -lapue

# Other UNIX utilities
AR = /usr/bin/ar
AWK = /usr/bin/awk

# Don't use obsolete suffix rules, instead use pattern rules.
.SUFFIXES:
