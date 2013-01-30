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

autoload -Uz colors compinit promptinit vcs_info
compinit
colors

zstyle ':vcs_info:*' unstagedstr '●'
zstyle ':vcs_info:*' stagedstr '✔'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' actionformats '(%F{10}%s->%F{3}%r%f|%F{14}%b%f%)%F{9}%u%F{10}%c%f(%F{9}%a%f)'
zstyle ':vcs_info:*' formats '(%F{10}%s->%F{3}%r%f|%F{14}%b%f%)%F{9}%u%F{10}%c%f'
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'
precmd () { vcs_info }

# Colored Cursor in Vi Mode
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

bindkey '^[[Z' reverse-menu-complete
bindkey '^A' vi-beginning-of-line
bindkey '^E' vi-end-of-line

setopt prompt_subst
PROMPT='%{$fg[red]%}%n%{$reset_color%}@%{$fg[green]%}%m%{$reset_color%}:%{$fg[yellow]%}%3~%{$reset_color%}${vcs_info_msg_0_}%#'
RPROMPT="[%{$fg_no_bold[yellow]%}%?%{$reset_color%}]"

if [ -e `which fortune` ]; then
  if [ -e /usr/share/fortune/discworld ]; then
    echo "`fortune discworld`"
  else
    echo "`fortune`"
  fi
fi
