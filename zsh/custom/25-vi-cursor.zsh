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
