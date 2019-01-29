#
#  ~/.bashrc
#
#  Configure what stays consistent across all my 
#  interctive bash shells.
#
#  No need to source /etc/bashrc (none in Arch)
#  /etc/profile sources /etc/bash.bashrc which
#  brings in shell completion.
#

export BASHRC_NON_INTERACTIVE=${BASHRC_NON_INTERACTIVE:=0}
export BASHRC_INTERACTIVE=${BASHRC_INTERACTIVE:=0}

if [[ $- != *i* ]]
then
    ((BASHRC_NON_INTERACTIVE++))
    # Don't configure anything, non-interactive
    # shells are responsible for their own configuration.

else
    ((BASHRC_INTERACTIVE++))
    # Make sure initial shell environment is well defined,
    # for both login shells and new terninal windows, even
    # if ~/.bash_profile not sourced.
    if [[  $(type -t bash_initconf_ran)  != function ]]
    then 
        # shellcheck source=/dev/null
        source ~/.bash_init
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
      kpsrbyftyyh07)
        HOST=galaga
        ;;
      kpsrbylzntj42)
        HOST=rygar
        ;;
      SCOTCh)
        if [[ $(uname) == CYGWIN_NT-10.0 ]]; then
            HOST=Cygwin
        elif [[ $(uname) == MINGW64_NT-10.0 ]]; then
            HOST=MinGW
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

    # 3 line prompt with pwd
    PS1='\n[\u@${HOST}: \w]\n\$ '
    PS2='> '
    PS3='> '
    PS4='+ '

    ## Aliases and Functions
    #  Need to use "shopt -s expand_aliases" in shell scripts.
    unalias rm 2> /dev/null
    unalias ls 2> /dev/null

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
    function ud() {
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
    function h2d () {
        echo "ibase=16; $*" | bc
    }
    function d2h () {
        echo "obase=16; $*" | bc
    }

    ## NVIDIA aliases

    # NVIDIA Daemon to keep card active when not running X-Windows
    alias nv-pd='sudo /usr/bin/nvidia-persistenced --user geoff --persistence-mode'
    # Activate and Deactivate respectfully.
    #    Communicates with above daemon if running, otherwise
    #    directly with card in a deprecated manner.
    alias nv-off='sudo /usr/bin/nvidia-smi -pm 0'
    alias nv-on='/usr/bin/sudo nvidia-smi -pm 1'

    ## GUI-land aliases and functions

    # Open Desktop or Windows file manager
    function fm () {
        local DiR="$1"
        [[ -n $DiR ]] || DiR="$PWD"
        if [[ $HOST =~ (Cygwin|MinGW) ]]
        then
            explorer "$(cygpath -w $DiR)"
        else
            xdg-open "$DiR" 
        fi
    }

    # Terminal which inherits environment of parent shell
    function tm () {
        if [[ $HOST =~ (Cygwin|MinGW) ]]; then
            ( mintty & )
        elif [[ -x /usr/bin/gnome-terminal ]]; then
            ( /usr/bin/gnome-terminal & )
        else
            ( /usr/bin/xterm & )
        fi
    }

    # PDF Reader
    function ev() {
      ( /usr/bin/evince "$@" >& /dev/null & )
    }

    # Firefox Browser
    function ff() {
      ( /usr/bin/firefox "$@" >& /dev/null & )
    }

    ## LibreOffice
    function lo () {
        ( /usr/bin/libreoffice & )
    }
    # LibreOffice writer
    function low () {
        ( /usr/bin/libreoffice --writer "$@" & )
    }

    ## ssh related functions and aliases
    function sshToSystem() {
      local system=$1
      local port=$2
      local user=$3
      /usr/bin/ssh -p "${port}" "${user}@${system}"
    }

    #  Single quotes intentional
    alias gauss17='sshToSystem ${GAUSS17}'
    alias maxwell4='sshToSystem ${MAXWELL4}'
    alias rygar='sshToSystem ${RYGAR}'
    alias galaga='sshToSystem ${GALAGA}'

    ## scp related functions and aliases
    function toSystem() {
      local system=$1
      local port=$2
      local user=$3
      shift 3
      /usr/bin/scp -P "${port}" -r "$@" "${user}@${system}:catch"
    }

    function fromSystem() {
      local system=$1
      local port=$2
      local user=$3
      shift 3
      local each
      for each in "$@"
      do
          /usr/bin/scp -P "${port}" -r "${user}@${system}:${each}" .
      done
    }

    #  Single quotes intentional
    alias toGauss17='toSystem ${GAUSS17}'
    alias fromGauss17='fromSystem ${GAUSS17}'

    alias toMaxwell4='toSystem ${MAXWELL4}'
    alias fromMaxwell4='fromSystem ${MAXWELL4}'

    alias toRygar='toSystem ${RYGAR}'
    alias fromRygar='fromSystem ${RYGAR}'

    alias toGalaga='toSystem ${GALAGA}'
    alias fromGalaga='fromSystem ${GALAGA}'

    ## Bash completion for stack (Haskell)
    #eval "$(stack --bash-completion-script stack)"

fi
