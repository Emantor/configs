# Set path to prompt
fpath+=( $ZSH/prompt )

# Autoload Promptinit AFTER fpath change
autoload promptinit; promptinit

# Set host-color for some workstations
zstyle ':prompt:*:Lu-Tze*'  host-color 046
zstyle ':prompt:*:Bibliothekar*'  host-color 088
zstyle ':prompt:*:tiffanyaching*'  host-color 032

# Git check-for-changes
zstyle ':vcs_info:*' check-for-changes true

# Hide if its the default user
[[ $USER == phoenix ]] && zstyle ':prompt:*:ps1' hide-user 1

if (( $+functions[prompt_phoenix_setup] )); then
  prompt phoenix
else
  echo "Prompt not found :("
fi
