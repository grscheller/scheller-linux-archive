#  Ksh shell functions and aliases
#
#  ~/.kshrc
#
# shellcheck shell=ksh
# shellcheck source=/dev/null

function ud
{
  typeset upDir=../
  if [[ $1 =~ ^[1-9][0-9]*$ ]]
  then
      for ((ii = 1; ii < $1; ii++))
      do
          upDir=../$upDir
      done
  fi
  cd $upDir || return
}

