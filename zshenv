# Env
export EDITOR=vim

[ -n "$TMUX" ] && export TERM=screen-256color

if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start --components=ssh)
    export SSH_AUTH_SOCK
fi

export PATH=$PATH:/home/phoenix/bin:/home/phoenix/.gem/ruby/2.1.0/bin
export ZSH=~/.zsh
export MANPAGER="/bin/sh -c \"col -b | vim -c 'set ft=man ts=8 nomod nolist nonu noma' -\""
