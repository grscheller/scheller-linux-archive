#  Bash shell functions and aliases
#
#  ~/.bashrc
#
#  Configure what stays consistent across
#  all my interctive bash sessions.
#
#  .bash_init hook to properly set up an
#  initial terminal environment.
#
#  Bash shells when launched by the terminal emulator
#  don't source ~/.bash_profile.  This is due to
#  the environment coming from a display managear
#  and not a login shell.

# shellcheck shell=bash
# shellcheck source=/dev/null

export BASHRC_NON_INTERACTIVE_LVL=${BASHRC_NON_INTERACTIVE_LVL:=0}
export BASHRC_INTERACTIVE_LVL=${BASHRC_INTERACTIVE_LVL:=0}
export BASH_PROFILE_LVL=${BASH_PROFILE_LVL:=0}
export BASH_INIT_LVL=${BASH_INIT_LVL:=0}

if [[ $- != *i* ]]
then
    ((BASHRC_NON_INTERACTIVE_LVL++))

    # Don't configure anything, non-interactive
    # shells are responsible for their own configuration.
else
    ((BASHRC_INTERACTIVE_LVL++))

    # Make sure an initial shell environment is well defined,
    # terminal windows are not descendant from login shells.
    if ((BASH_INIT_LVL < 1)) || ((BASH_PROFILE_SOURCED == 1)) 
    then 
        source ~/.bash_init
        unset BASH_PROFILE_SOURCED 
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

    ## Assign more realistic inactivity timeout period (8 hours)
    [[ -n $TMOUT ]] && TMOUT=28800

    ## Bash customizations when running interactively
    set -o pipefail  # Return right most nonzero error, otherwise 0.
    shopt -s extglob  # Turn on extended pattern matching.
    shopt -s checkwinsize
    shopt -s checkhash # Checks if hashed cmd exists, otherwise search path.
    # Command line history editing and terminal title
    shopt -s cmdhist     # Store multiline commands as single entry
    shopt -s lithist     # in history with embedded whitespace.
    shopt -s histappend    # Append, don't replace history file.
    HISTSIZE=10000
    HISTFILESIZE=10000
    HISTCONTROL="ignoredups"

    ## Asign more memorable names to hosts.
    ## Also, some cygwin/mysys2/mingwing configurations.
    export HOST=${HOSTNAME%%.*}
    case $HOST in
      rvsllschellerg2) HOST=voltron ;;
      rvswlschellerg1) HOST=koala ;;
      rvswlwojcikj1)   HOST=littlejohn ;;
      *) if [[ $(uname) =~ ^CYGWIN ]]; then
             HOST=CYGWIN
             export CYGWIN=winsymlinks:nativestrict
         elif [[ $(uname) =~ ^MSYS ]]; then
             HOST=MSYS2
             export MSYS=winsymlinks:nativestrict
         elif [[ $(uname) =~ ^MINGW64 ]]; then
             HOST=MINGW64
             export MSYS=winsymlinks:nativestrict
         elif [[ $(uname) =~ ^MINGW32 ]]; then
             HOST=MINGW32
             export MSYS=winsymlinks:nativestrict
         fi
         ;;
    esac

    ## Save history whenever prompt displayed
    ## and update terminal window title.
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

    ## 3 line prompt
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
    alias ll='ls -ltr'
    alias lh='ls -ltrh'
    alias lla='ls -ltra'
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

    # ax - archive extractor
    # usage: ax <file>
    ax ()
    {
      if [[ -f $1 ]]
      then
          case $1 in
            *.tar)     tar -xvf "$1"                        ;;
            *.tar.bz2) tar -xjvf "$1"                       ;;
            *.tbz2)    tar -xjvf "$1"                       ;;
            *.tar.gz)  tar -xzvf "$1"                       ;;
            *.tgz)     tar -xzvf "$1"                       ;;
            *.tar.Z)   tar -xZvf "$1"                       ;;
            *.gz)      gunzip "$1"                          ;;
            *.bz2)     bunzip2 "$1"                         ;;
            *.zip)     unzip "$1"                           ;;
            *.Z)       uncompress "$1"                      ;;
            *.rar)     unrar x "$1"                         ;;
            *.tar.xz)  xz -dc "$1" | tar -xvf -             ;;
            *.tar.7z)  7za x -so "$1" | tar -xvf -          ;;
            *.7z)      7z x "$1"                            ;;
            *.cpio)    cpio -idv < "$1"                     ;;
            *) echo "ax: error: '$1' unknown file type" >&2 ;;
          esac
      else
          if [[ -n $1 ]]
          then
              echo "ax: error: '$1' is not a file" >&2
          else
              echo "ax: error: No file argument given" >&2
          fi
      fi
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
           printf "tm: warning: terminat emulator not found\n" >&2
       fi
    }

    # PDF Reader
    ev ()
    {
      ( /usr/bin/evince "$@" &>/dev/null & )
    }

    # LBRY AppImage
    lbry ()
    {
      ( ~/build/AppImages/LBRY_0.45.2.AppImage &>/dev/null & )
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

    # Restart SSH key-agent and add your private
    # key located here: ~/.ssh/id_rsa
    # Useful when using SSH authentication with GITHUB.
    alias addkey='eval $(ssh-agent) && ssh-add'

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

    fromSystem ()
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

      local each
      for each in "$@"
      do
          $SCP -P "${port}" -r "${user}@${system}:${each}" .
      done
    }

    #  Single quotes intentional
    alias voltron='sshToSystem ${VOLTRON}'
    alias toVoltron='toSystem ${VOLTRON}'
    alias fromVoltron='fromSystem ${VOLTRON}'

    alias koala='sshToSystem ${KOALA}'
    alias toKoala='toSystem ${KOALA}'
    alias fromKoala='fromSystem ${KOALA}'

    alias littlejohn='sshToSystem ${LITTLEJOHN}'
    alias toLittlejohn='toSystem ${LITTLEJOHN}'
    alias fromLittleJohn='fromSystem ${LITTLEJOHN}'

    alias gauss17='sshToSystem ${GAUSS17}'
    alias toGauss17='toSystem ${GAUSS17}'
    alias fromGauss17='fromSystem ${GAUSS17}'

    alias maxwell4='sshToSystem ${MAXWELL4}'
    alias toMaxwell4='toSystem ${MAXWELL4}'
    alias fromMaxwell4='fromSystem ${MAXWELL4}'

    ## Setup JDK on Arch
    archJDK ()
    {
      local version=$1

      if [[ ! $version =~ ^[0-9]+$ ]] 
      then
         echo "Malformed JDK version number: \"$version\""
         return
      fi
      
      if [[ -d /usr/lib/jvm/java-${version}-openjdk ]]
      then
          export JAVA_HOME=/usr/lib/jvm/java-${version}-openjdk
          if [[ $PATH =~ ^/usr/lib/jvm/java-[0-9]+-openjdk/bin: ]]
          then
              PATH=${PATH#[^:]*:}
          fi
          PATH=$JAVA_HOME/bin:$PATH
      else
          echo "No JDK found for Java \"$version\" in the"
          echo "standard location for Arch: /usr/lib/jvm/"
      fi
    }

    ## Bash completion for stack (Haskell)
    #eval "$(stack --bash-completion-script stack)"

    ## Configure Anaconda3 Python Distribution
    if [[ -d ~/opt/anaconda3 ]]
    then
        # On Windows 10, Anaconda is installed into the
        # MSYS2/Anaconda Prompt world.  We must tell
        # Cygwin to ignore LF characters or the conda
        # shell function will fail.
        if [[ $OSTYPE == cygwin ]]
        then
            set -o igncr
        fi
        source ~/opt/anaconda3/etc/profile.d/conda.sh
    fi

fi
