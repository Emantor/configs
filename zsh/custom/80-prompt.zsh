# Standard Prompt
setopt prompt_subst
PROMPT='%{$fg[red]%}%n%{$reset_color%}@%{$fg[green]%}%m%{$reset_color%}:%{$fg[yellow]%}%3~%{$reset_color%}${vcs_info_msg_0_}%#'
RPROMPT="[%{$fg_no_bold[yellow]%}%?%{$reset_color%}]"
