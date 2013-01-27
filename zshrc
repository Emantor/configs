# Lines configured by zsh-newuser-install

HISTFILE=~/.histfile
HISTSIZE=32767
SAVEHIST=32767
setopt appendhistory autocd extendedglob
unsetopt notify
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/phoenix/.zshrc'

# Complete with menu
zstyle ':completion:*' menu select

autoload -Uz colors compinit promptinit
compinit
colors

# source zshrc.sh for git-prompt
source /home/phoenix/.zsh/zshrc.sh

zle-keymap-select () {
  if [ $TERM = "rxvt-256color" ]; then
    if [ $KEYMAP = vicmd ]; then
      echo -ne "\033]12;Red\007"
    else
      echo -ne "\033]12;Grey\007"
    fi
  fi
}
zle -N zle-keymap-select
zle-line-init () {
  zle -K viins
    if [ $TERM = "rxvt-256color" ]; then
      echo -ne "\033]12;Grey\007"
    fi
}
zle -N zle-line-init
 # VI Keybindings
bindkey -v
# Remap ESC to jj
bindkey -M viins 'jj' vi-cmd-mode
#if hostname == keks; then
#  PROMPT='%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m%{$reset_color%}:%{$fg[yellow]%}%3~%{$reset_color%}$(git_super_status)%#'
#else
  PROMPT='%{$fg[red]%}%n%{$reset_color%}@%{$fg[green]%}%m%{$reset_color%}:%{$fg[yellow]%}%3~%{$reset_color%}$(git_super_status)%#'
#fi
RPROMPT="[%{$fg_no_bold[yellow]%}%?%{$reset_color%}]"

have_fortune=`which fortune`
if [ -e have_fortune ]; then
    echo ""
    fortune 
    echo ""
fi
