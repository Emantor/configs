# Env
export EDITOR=vim

[ -n "$TMUX" ] && export TERM=screen-256color

if pgrep xmonad >/dev/null;then
    eval $(gnome-keyring-daemon --start --components=ssh)
    export SSH_AUTH_SOCK
fi

export PATH=$PATH:home/phoenix/bin:/home/phoenix/.gem/ruby/2.1.0/bin:/home/phoenix/.cabal/bin
export ZSH=~/.zsh
export GOPATH=$HOME/work/go
