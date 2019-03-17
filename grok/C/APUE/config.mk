# APUE Project Configuration
#
# This is NOT a recursive Make build.  The build is
# done from the root APUE directory.

# APUE project defaults and directory stucture 
LIBDIR = lib
INCLUDE = include
BIN = bin

# Library and headerfile to support an implementation of
# W. Richard Steven's API for UNIX System Programming.
LIBAPUE_A = $(LIBDIR)/libapue.a
APUE_H = $(INCLUDE)/apue.h

# All feature macros defined in apue.h

# Compiler flags for specific OS's.
# Here is where to select dialect of C Language, and define
# macro flags for minimal conditional complilation.
LINUX_CFLAGS := -std=c99 -DLINUX
CYGWIN_CFLAGS := -std=c99 -DCYGWIN
FREEBSD_CFLAGS := -std=c99 -DBSD             # untested
MACOS_CFLAGS := -std=c99 -DMACOS             # untested

LINUX_ANSI_CFLAGS := -ansi -DLINUX           # will fail
LINUX_FORTIFY_CFLAGS := -std=c99 -DLINUX -D_FORTIFY_SOURCE=1 -O2

# Uncomment one to select for your system
SYSTEM_CFLAGS := $(LINUX_CFLAGS)
# SYSTEM_CFLAGS := $(CYGWIN_CFLAGS)

# C compiler configuration
CC = gcc
CPPFLAGS = 
CFLAGS = $(CPPFLAGS) $(SYSTEM_CFLAGS) -Wall -I$(INCLUDE)
LDFLAGS = -L$(LIBDIR) -lapue

ifeq ($(SYSTEM_CFLAGS),$(CYGWIN_CFLAGS))
    EXT := .exe
else
    EXT :=
endif	

# Other UNIX utilities
AR = ar
AWK = awk

# Don't use obsolete suffix rules, instead use pattern rules.
.SUFFIXES:
