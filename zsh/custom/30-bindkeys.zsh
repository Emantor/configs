 # VI Keybindings
bindkey -v
# Remap ESC to jj
bindkey -M viins 'jj' vi-cmd-mode
# Reverse search
bindkey '^[[Z' reverse-menu-complete
# Emacs Bindings
bindkey '^R' history-incremental-search-backward
bindkey '^A' vi-beginning-of-line
bindkey '^E' vi-end-of-line
# Del-Button
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char

bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward
