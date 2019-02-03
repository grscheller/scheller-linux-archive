#  Bash shell functions and aliases
#
#  ~/.bashrc
#
#  Configure what stays consistent across
#  all my interctive bash sessions.
#
#  .bash_init hook to properly set up an
#  initial terminal environment,  Most
#  Desktop Environments launched from a
#  display managers never source ~/.bash_profile
# 

# shellcheck shell=bash
# shellcheck source=/dev/null

export BASHRC_NON_INTERACTIVE=${BASHRC_NON_INTERACTIVE:=0}
export BASHRC_INTERACTIVE=${BASHRC_INTERACTIVE:=0}

if [[ $- != *i* ]]
then
    # Don't configure anything, non-interactive
    # shells are responsible for their own configuration.
    :
else
    # Make sure an initial shell environment is well defined,
    # for both login shells and new terminal windows, even
    # if ~/.bash_profile not sourced.
    ((BASHRC_INTERACTIVE++))

    if [[ -f .bash_init ]] && [[ ${BASH_INIT_SOURCED} != bash_init_sourced ]] 
    then 
        source .bash_init
    fi

    # Mechanism used on Redhat & Redhat derived systems
    # to configure system-wide aliases and functions.
    # Note: Important for bash completion and D-Bus configuration.
    #       Redhat /etc/bashrc reruns all the /etc/profile.d/*.sh
    #       scripts.
    # Note: Debian derived systems and Arch Linux compile bash with
    #       an option (-DSYS_BASHRC) to source /etc/bash.bashrc
    #       before this file.
    if [[ -f /etc/bashrc ]]
    then
        source /etc/bashrc
    fi

    # Reload Bash completion scripts if not already done above.
    # Note: usually done in either /etc/bashrc or /etc/bash.bashrc
    # Note: the Cygwin environment startup scripts are broken
    if [[ $(type -t __parse_options) != function ]]
    then
        if [[ -f /usr/share/bash-completion/bash_completion ]]; then
            source /usr/share/bash-completion/bash_completion 
        elif [[ -f /usr/share/bash-completion/bash_completion.sh ]]; then
            source /usr/share/bash-completion/bash_completion.sh
        elif [[ -f /etc/bash_completion ]]; then
            source /etc/bash_completion
        elif [[ -f /etc/bash_completion.sh ]]; then
            source /etc/bash_completion.sh
        fi
    fi

    ## Make sure git asks for passwords on the command line
    unset SSH_ASKPASS

    ## Timeout bash sessions after about 8 hours of inactivity
    TMOUT=60000

    ## Bash customizations when running interactively
    set -o notify  # Do not wait until next prompt to report bg jobs status.
    set -o pipefail  # Return right most nonzero error, otherwise 0.
    shopt -s extglob  # Turn on extended pattern matching.
    shopt -s checkwinsize
    shopt -s checkhash # Checks if hashed cmd exists, otherwise search path.
    # Command line history editing and terminal title
    #set -o vi       # vi editing-mode, set here if not in ~/.inputrc
    shopt -s cmdhist     # Store multiline commands as single entry
    shopt -s lithist     # in history with embedded whitespace.
    shopt -s histappend    # Append, don't replace history file.
    HISTSIZE=5000
    HISTFILESIZE=10000
    HISTCONTROL="ignoredups"

    ## Assign more memorable names to hosts
    export HOST=${HOSTNAME%%.*}
    case $HOST in
      rvsllschellerg2)
        HOST=voltron
        ;;
      kprswvbylnzjt52)
        HOST=galaga
        ;;
      rvsllmonetd1)
        HOST=evergarden
        ;;
      rvswlschellerg1)
        HOST=koala
        ;;
      SCOTCh)
        if [[ $(uname) == CYGWIN_NT-10.0 ]]; then
            HOST=Cygwin
        elif [[ $(uname) == MINGW64_NT-10.0 ]]; then
            HOST=MinGW
        elif [[ $(uname) == MSYS_NT-10.0 ]]; then
            HOST=MSYS2
        fi
        ;;
    esac

    ## Save history whenever prompt displayed
    case $TERM in
      xterm*|rxvt*|urxvt*|kterm*|gnome*)
        PROMPT_COMMAND='history -a; echo -ne "\033]0;${USER}@${HOST}\007"'
        ;;
      screen)
        PROMPT_COMMAND='history -a; echo -ne "\033_${USER}@${HOST}\033\\"'
        ;;
      *)
        PROMPT_COMMAND='history -a'
        ;;
    esac

    ## 3 line prompt with pwd
    PS1='\n[\u@${HOST}: \w]\n\$ '
    PS2='> '
    PS3='> '
    PS4='+ '

    ## Aliases and Functions
    unalias rm 2>-
    unalias ls 2>-
    unalias grep 2>-
    unalias egrep 2>-
    unalias fgrep 2>-

    alias lc='ls --color=auto'
    alias l1='ls -1'
    alias la='ls -a'
    alias ll='ls -ltrh'
    alias lla='ls -ltrah'
    alias l.='ls -dA .* --color=auto'

    alias pst="ps axjf | sed -e '/^ PPID.*$/d' -e's/.*:...//'"
    alias which='alias | /usr/bin/which --tty-only --read-alias --show-dot --show-tilde'

    # Pull down a subset of a website
    alias Wget='/usr/bin/wget -p --convert-links -e robots=off'
    # Pull down more -- Not good for large websites
    alias WgetMirror='/usr/bin/wget --mirror -p --convert-links -e robots=off'

    # pop up multiple directories
    ud ()
    {
      local upDir=../
      if [[ $1 =~ ^[1-9][0-9]*$ ]]
      then
        for ((ii = 1; ii < $1; ii++))
        do
          upDir=../$upDir
        done
      fi
      cd $upDir || return
    }

    # Convert between hex and dec
    h2d ()
    {
      echo "ibase=16; $*" | bc
    }

    d2h ()
    {
      echo "obase=16; $*" | bc
    }

    ## NVIDIA aliases

    # NVIDIA Daemon to keep card active when not running X-Windows
    alias nv-pd='sudo /usr/bin/nvidia-persistenced --user geoff --persistence-mode'
    # Activate and Deactivate respectfully.
    #    Communicates with above daemon if running, otherwise
    #    directly with card in a deprecated manner.
    alias nv-off='sudo /usr/bin/nvidia-smi -pm 0'
    alias nv-on='sudo nvidia-smi -pm 1'

    ## GUI-land aliases and functions

    # Open Desktop or Windows file manager
    fm ()
    {
      local DiR="$1"
      [[ -n $DiR ]] || DiR="$PWD"
      if [[ $HOST =~ (Cygwin|MinGW|MSYS2) ]]
      then
          explorer "$(cygpath -w "$DiR")"
      else
          xdg-open "$DiR" 
      fi
    }

    # Terminal which inherits environment of parent shell
    tm ()
    {
       if [[ $HOST =~ (Cygwin|MinGW|MSYS2) ]]; then
           ( mintty & )
       elif [[ -x /usr/bin/gnome-terminal ]]; then
           ( /usr/bin/gnome-terminal >&- & )
       else
           ( /usr/bin/xterm >/dev/null 2>&1 & )
       fi
    }

    # PDF Reader
    ev ()
    {
      ( /usr/bin/evince "$@" &>- & )
    }

    # Firefox Browser
    ff ()
    {
      ( /usr/bin/firefox "$@" &>- & )
    }

    ## LibreOffice
    lo ()
    {
      ( /usr/bin/libreoffice & )
    }
    
    # LibreOffice writer
    low ()
    {
      ( /usr/bin/libreoffice --writer "$@" & )
    }

    ## ssh related functions and aliases

    # pkinit alias for HPC
    alias pkhpc='pkinit schelleg@HPCMP.HPC.MIL'

    sshToSystem ()
    {
      local system=$1
      local port=$2
      local user=$3
      local hpc=$4

      local SSH
      if [[ $hpc == 'no' ]]
      then
          # Use system version of ssh
          SSH=/usr/bin/ssh
      else
          # Use HPCMP version of ssh from $PATH
          SSH=ssh
      fi

      $SSH -p "${port}" "${user}@${system}"
    }

    #  Single quotes intentional
    alias gauss17='sshToSystem ${GAUSS17}'
    alias maxwell4='sshToSystem ${MAXWELL4}'
    alias voltron='sshToSystem ${VOLTRON}'
    alias evergarden='sshToSystem ${EVERGARDEN}'
    alias koala='sshToSystem ${KOALA}'
    alias galaga='sshToSystem ${GALAGA}'
    alias topaz='sshToSystem ${TOPAZ}'
    alias ust='sshToSystem ${UST}'
    alias zambia='sshToSystem ${ZAMBIA}'

    ## scp related functions and aliases
    toSystem ()
    {
      local system=$1
      local port=$2
      local user=$3
      local hpc=$4
      shift 4

      local SCP
      if [[ $hpc == 'no' ]]
      then
          # Use system version of scp
          SCP=/usr/bin/scp
      else
          # Use HPCMP version of scp from $PATH
          SCP=scp
      fi

      $SCP -P "${port}" -r "$@" "${user}@${system}:catch"
    }

    fromSystem () {
      local system=$1
      local port=$2
      local user=$3
      local hpc=$4
      shift 4

      local SCP
      if [[ $hpc == 'no' ]]
      then
          # Use system version of scp
          SCP=/usr/bin/scp
      else
          # Use HPCMP version of scp from $PATH
          SCP=scp
      fi

      local each
      for each in "$@"
      do
          $SCP -P "${port}" -r "${user}@${system}:${each}" .
      done
    }

    #  Single quotes intentional
    alias toGauss17='toSystem ${GAUSS17}'
    alias fromGauss17='fromSystem ${GAUSS17}'

    alias toMaxwell4='toSystem ${MAXWELL4}'
    alias fromMaxwell4='fromSystem ${MAXWELL4}'

    alias toVoltron='toSystem ${VOLTRON}'
    alias fromVoltron='fromSystem ${VOLTRON}'

    alias toEvergarden='toSystem ${EVERGARDEN}'
    alias fromEvergarden='fromSystem ${EVERGARDEN}'

    alias toKoala='toSystem ${KOALA}'
    alias fromKoala='fromSystem ${KOALA}'

    alias toGalaga='toSystem ${GALAGA}'
    alias fromGalaga='fromSystem ${GALAGA}'

    alias toTopaz='toSystem ${TOPAZ}'
    alias fromTopaz='fromSystem ${TOPAZ}'

    alias toUST='toSystem ${UST}'
    alias fromUST='fromSystem ${UST}'

    alias toZambia='toSystem ${ZAMBIA}'
    alias fromZambia='fromSystem ${ZAMBIA}'

    ## Bash completion for stack (Haskell)
    #eval "$(stack --bash-completion-script stack)"

fi
