 # VI Keybindings
bindkey -v
# Remap ESC to jj
bindkey -M viins 'jj' vi-cmd-mode

bindkey '^[[Z' reverse-menu-complete
bindkey '^R' history-incremental-search-backward
bindkey '^A' vi-beginning-of-line
bindkey '^E' vi-end-of-line
