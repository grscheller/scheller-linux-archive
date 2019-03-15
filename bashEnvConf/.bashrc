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
    # if ~/.bash_profile was never sourced.
    ((BASHRC_INTERACTIVE++))

    if [[ ! -v BASH_INIT_SOURCED ]] || [[ -v BASH_PROFILE_RESOURCED ]] 
    then 
        source ~/.bash_init
    fi

    # Mechanism used on Redhat & Redhat derived systems
    # to configure system-wide aliases and functions.
    #
    # Note: Important for bash completion and desktop configuration.
    #       Redhat /etc/bashrc reruns all the /etc/profile.d/*.sh
    #       scripts.
    # Note: Debian derived systems and Arch Linux compile bash with
    #       an option (-DSYS_BASHRC) to source /etc/bash.bashrc
    #       before user's ~/.bashrc file.
    #
    if [[ -f /etc/bashrc ]]
    then
        source /etc/bashrc
        if [[ -f /etc/redhat-release ]]
        then
            # "Domain Users" group way too broad on CentOS 7 at work 
            umask u=rwx,g=,o=
        fi
    fi

    # Reload Bash completion scripts if not already done above.
    #
    # Note: Usually done in either /etc/bashrc or /etc/bash.bashrc.
    # Note: Cygwin environment startup scripts are broken, without
    #       this bash completion only works in a top level shell.
    #
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
      rvsllsherwoodj1)
        HOST=sherwood
        ;;
      rvswlwojcikj1)
        HOST=littlejohn
        ;;
      rvswlcrabtreep1)
        HOST=trex
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

    ## Setup or tear down Network Manager proxies when necessary
    #  - mostly for CentOS 7 work environment
    #  - assumes you have manualyl set up the proxies in the Settings applet
    #  - sets up Proxy environmental variables for current shell
    #  - enable proxy for Network Manager if on the console
    #  - might need to change this if I ever start running remote desktops
    proxyUp () {
        [[ -f ~/.proxy_env ]] && source ~/.proxy_env
        if [[ $DISPLAY =~ ^:[0-9]$ ]] && [[ -x /usr/bin/gsettings ]]
        then
            gsettings set org.gnome.system.proxy mode manual
        fi
    }

    downProxy () {
        unset http_proxy HTTP_PROXY https_proxy HTTPS_PROXY no_proxy NO_PROXY 
        if [[ $DISPLAY =~ ^:[0-9]$ ]] && [[ -x /usr/bin/gsettings ]]
        then
            gsettings set org.gnome.system.proxy mode none
        fi
    }

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
    unalias rm 2>&-
    unalias ls 2>&-
    unalias grep 2>&-
    unalias egrep 2>&-
    unalias fgrep 2>&-

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

    # Convert from dec and hex
    d2h ()
    {
      echo "obase=16; $*" | /usr/bin/bc
    }

    # Convert from hex and dec, use capital A-F
    h2d ()
    {
      echo "ibase=16; $*" | /usr/bin/bc
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
           ( /usr/bin/gnome-terminal >&- )
       elif [[ -x /usr/bin/xterm ]]; then
           ( /usr/bin/xterm >/dev/null 2>&1 & )
       else
           printf "tm: warning: suitable terminator not found\n" >&2
       fi
    }

    # PDF Reader
    ev ()
    {
      ( /usr/bin/evince "$@" &>/dev/null & )
    }

    # Firefox Browser
    ff ()
    {
      local FIREFOX=firefox
      [[ -x /usr/bin/firefox ]] && FIREFOX=/usr/bin/firefox
      [[ -x /usr/local/bin/firefox ]] && FIREFOX=/usr/local/bin/firefox
      ( $FIREFOX "$@" >&- 2>&- & )
    }

    # Google Browser
    gb ()
    {
      local CHROME=chrome
      [[ -x /usr/bin/chrome ]] && CHROME=/usr/bin/chrome
      [[ -x /usr/local/bin/chrome ]] && CHROME=/usr/local/bin/chrome
      [[ -x /opt/google/chrome/chrome ]] && CHROME=/opt/google/chrome/chrome
      ( $CHROME "$@" >&- 2>&- & )
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
    pkhpc ()
    {
        module load hpc
        pkinit schelleg@HPCMP.HPC.MIL
    }

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
    alias gauss17='sshToSystem ${GAUSS17}'
    alias toGauss17='toSystem ${GAUSS17}'
    alias fromGauss17='fromSystem ${GAUSS17}'

    alias maxwell4='sshToSystem ${MAXWELL4}'
    alias toMaxwell4='toSystem ${MAXWELL4}'
    alias fromMaxwell4='fromSystem ${MAXWELL4}'

    alias voltron='sshToSystem ${VOLTRON}'
    alias toVoltron='toSystem ${VOLTRON}'
    alias fromVoltron='fromSystem ${VOLTRON}'

    alias sherwood='sshToSystem ${SHERWOOD}'
    alias toSherwood='toSystem ${SHERWOOD}'
    alias fromSherwood='fromSystem ${SHERWOOD}'

    alias littlejohn='sshToSystem ${LITTLEJOHN}'
    alias toLittlejohn='toSystem ${LITTLEJOHN}'
    alias fromLittlejohn='fromSystem ${LITTLEJOHN}'

    alias trex='sshToSystem ${TREX}'
    alias toTrex='toSystem ${TREX}'
    alias fromTrex='fromSystem ${TREX}'

    alias evergarden='sshToSystem ${EVERGARDEN}'
    alias toEvergarden='toSystem ${EVERGARDEN}'
    alias fromEvergarden='fromSystem ${EVERGARDEN}'

    alias koala='sshToSystem ${KOALA}'
    alias toKoala='toSystem ${KOALA}'
    alias fromKoala='fromSystem ${KOALA}'

    alias galaga='sshToSystem ${GALAGA}'
    alias toGalaga='toSystem ${GALAGA}'
    alias fromGalaga='fromSystem ${GALAGA}'

    alias topaz='sshToSystem ${TOPAZ}'
    alias toTopaz='toSystem ${TOPAZ}'
    alias fromTopaz='fromSystem ${TOPAZ}'

    alias ust='sshToSystem ${UST}'
    alias toUST='toSystem ${UST}'
    alias fromUST='fromSystem ${UST}'

    alias zambia='sshToSystem ${ZAMBIA}'
    alias toZambia='toSystem ${ZAMBIA}'
    alias fromZambia='fromSystem ${ZAMBIA}'

    ## Bash completion for stack (Haskell)
    #eval "$(stack --bash-completion-script stack)"

    ## Configure Anaconda3 Python Distribution
    if [[ -d ~/opt/anaconda3 ]]
    then
        source ~/opt/anaconda3/etc/profile.d/conda.sh
    fi

fi
