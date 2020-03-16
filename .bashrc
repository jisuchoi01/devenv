# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# Console Style Constants
RST="\[\e[0m\]"           # Reset Styles
BOLD="\[\e[1m\]"          # Bold
UL="\[\e[4m\]"            # Underline
HIGHLIGHT="\[\e[7m\]"            # Highlight (inverse)
FG_BLACK="\[\e[90m\]"     # Foreground black
FG_RED="\[\e[91m\]"       # Foreground red
FG_GREEN="\[\e[92m\]"     # Foreground green
FG_YELLOW="\[\e[93m\]"    # Foreground yellow
FG_BLUE="\[\e[94m\]"      # Foreground blue
FG_MAGENTA="\[\e[95m\]"   # Foreground magenta
FG_CYAN="\[\e[96m\]"      # Foreground cyan
FG_WHITE="\[\e[97m\]"     # Foreground white
BG_BLACK="\[\e[100m\]"    # Background black
BG_RED="\[\e[101m\]"      # Background red
BG_GREEN="\[\e[102m\]"    # Background green
BG_YELLOW="\[\e[103m\]"   # Background yellow
BG_BLUE="\[\e[104m\]"     # Background blue
BG_MAGENTA="\[\e[105m\]"  # Background magenta
BG_CYAN="\[\e[106m\]"     # Background cyan
BG_WHITE="\[\e[107m\]"    # Background white

if [ "$color_prompt" = yes ]; then
		PS1="\n${FG_MAGENTA}[\d \t]${RST}\`if [ \$? != 0 ]; then echo ' ${FG_RED}${BOLD}${HIGHLIGHT}!${RST}'; fi\` ${FG_YELLOW}\
[job]: \j [dir]: \w ${RST}\n${debian_chroot:+($debian_chroot)}${FG_GREEN}${BOLD}\u${RST}${FG_WHITE}@${RST}${FG_CYAN}\h${RST}\$ "

#    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias e='emacs -nw --no-splash'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QR_IM_MODULE=ibus

export XAUTHORITY=~/.Xauthority
export DISPLAY=localhost:0.0

export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games
export ANACONDA3_PATH=/home/dsparch/anaconda3/bin
export PATH=$PATH:$ANACONDA3_PATH
#:/usr/local/games:/mnt/c/Program Files (x86)/Common Files/Oracle/Java/javapath:/mnt/c/Program Files (x86)/NAT #Service:/mnt/c/Windows/system32:/mnt/c/Windows:/mnt/c/Windows/System32/Wbem:/mnt/c/Windows/System32/WindowsPowerShell/v1.0/:/mnt/c/Windows/System32/OpenSSH/:/mnt/c/Users/dspar/AppData/Local/Microsoft/WindowsApps:/mnt/c/Users/dspar/AppData/Local/Programs/Microsoft VS #Code/bin:/mnt/c/Users/dspar/Anaconda3/Library/bin:/mnt/c/Users/dspar/Anaconda3/Scripts:/mnt/c/Users/dspar/Anaconda3:/mnt/c/Program Files/Bandizip/:/snap/bin

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/dsparch/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/dsparch/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/dsparch/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/dsparch/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

set bell-style none
