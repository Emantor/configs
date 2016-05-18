# Env
export EDITOR='emacsclient -t -a='

[ -n "$TMUX" ] && export TERM=screen-256color

if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

export PATH=$PATH:home/phoenix/bin:/home/phoenix/.gem/ruby/2.1.0/bin:/home/phoenix/.cabal/bin
export ZSH=~/.zsh
export GOPATH=$HOME/work/go
