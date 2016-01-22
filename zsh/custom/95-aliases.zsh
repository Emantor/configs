# Aliases
alias ls='ls --color=auto -h'
alias :q='exit'
alias iptables='echo "Alias: sudo iptables" && sudo iptables'
alias rmg='ssh rmg'
alias mg='ssh mg'
alias s='ssh'

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

alias mpv-s='systemd-inhibit --what=handle-lid-switch:idle --who=mpv --why="Video is playing" mpv --x11-name seperate_mpv'

alias fuck='eval $(thefuck $(fc -ln -1 | tail -n 1)); fc -R'

ec() {
    emacsclient -a= -t $@
}

alias dmesg='dmesg -T'

# File Aliases
alias -s tex=ec
alias -s c=ec
alias -s h=ec
alias -s txt=ec
alias -s mkv=mpv

+(){ sudo "$*"; }
++(){ sudo -i; }

function sysrestart() { sudo systemctl restart $1; journalctl -n 10 -u $1 --no-pager; }

function wlan() {
    if [ $1 = "on" ];
    then sudo systemctl start netctl-auto@wlan;
    else if [ $1 = "off" ];
         then sudo systemctl stop netctl-auto@wlan;
         fi;
    fi;
}
