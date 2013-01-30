# 
zstyle ':vcs_info:*' unstagedstr '●'
zstyle ':vcs_info:*' stagedstr '✔'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' actionformats '(%F{10}%s->%F{3}%r%f|%F{14}%b%f%)%F{9}%u%F{10}%c%f(%F{9}%a%f)'
zstyle ':vcs_info:*' formats '(%F{10}%s->%F{3}%r%f|%F{14}%b%f%)%F{9}%u%F{10}%c%f'
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'
precmd () { vcs_info }
