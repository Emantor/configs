# Aliases
alias ls='ls --color=auto -h'
alias :q='exit'
alias iptables='echo "Alias: sudo iptables" && sudo iptables'

# Aliases für Terra Netzwerk
alias zephyda='ssh -p 8022 root@zephyda'
alias nathan='ssh phoenix@nathan'
alias biblio='ssh phoenix@bibliothekar'
alias ncmpc='ncmpc -c'

# Alias für Infam
alias willi='ssh -C -X rouven@willi'
alias bibo='ssh -C -X rouven@bibo'
alias sfb880='ssh rouven@sfb880.tu-bs.de'
alias infam='ssh rouven@infam.rz.tu-bs.de'

# Aliases für arch
alias pacman='sudo pacman'
alias netcfg='sudo netcfg'

# Aliases für Stratum0
alias spacekiste='ssh stratum0@192.168.178.196'

# Alias für vserver
alias tiffany='ssh tiffanyaching.emantor.de'

# Alias für mpv
alias mpv='systemd-inhibit --what=handle-lid-switch:idle --who=mpv --why="Video is playing" mpv'

# File Aliases
alias -s tex=vim
alias -s c=vim
alias -s h=vim
alias -s txt=vim
alias -s mkv=mpv

+(){ sudo "$*"; }
++(){ sudo -i; }
