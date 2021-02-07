#!/bin/bash
#
# Drill down through $PATH to look for files or directories.
# Like the ksh builtin whence, except it does not stop
# after finding the first instance.  Handles spaces in both
# filenames and directories on $PATH.  Also, shell patterns
# are supported.
#
# Usage: digpath [-q] [-h] file1 file2 ...
#        digpath [-q] [-h] shell_extended_glob_pattern
#
# Options: -q: quite
#          -h: help
#
# Returns: 0 if a file was found on $PATH
#          1 if no file found on $PATH
#          2 if help option or an invalid option given
#
#   Written by Geoffrey Scheller
#   See: https://github.com/grscheller/scheller-linux-archive/env
#

usage () {
  printf 'Usage: digpath [-q] file1 file2 ...\n' >&2
  printf '       digpath [-q] shell_pattern\n'   >&2
  printf '       digpath [-h]\n'                 >&2
}

quite_flag=
while getopts ":hq" opt
do
  case $opt in
    q) quite_flag=1
       ;;
    h) usage
       exit 2
       ;;
    ?) printf 'Error: invalid option %s\n' "$OPTARG"  >&2 
       usage
       exit 2
       ;;
  esac
done

shopt -s extglob   # Turn on extended pattern matching
IFS=':'

ii=0  # for array index
for File in "$@"
do
   [[ -z "$File" ]] && continue
   for Dir in $PATH
   do
      [[ ! -d "$Dir" ]] && continue
      if [[ -d "$Dir" ]]
      then
          for match in $Dir/$File
          do
              [[ -f $match ]] && FileList[((ii++))]="$match"
          done
      fi
   done
done

[[ -z $quite_flag ]] && printf '%s\n' "${FileList[@]}"

if ((${#FileList[@]} > 0))
then
    exit 0
else
    exit 1
fi
