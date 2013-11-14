# Load zsh files. These are loaded in order (01-99), local overwrites custom.
for config_file in $ZSH/(local|custom)/*.zsh(Noe!'REPLY=${REPLY:t}'!oe!'[[ $REPLY == *local* ]] && REPLY=0 || REPLY=1'!); source $config_file
if  [[ -e /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
  source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
