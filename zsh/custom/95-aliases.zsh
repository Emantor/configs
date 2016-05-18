# Aliases
alias ls='ls --color=auto -h'
alias :q='exit'
alias iptables='echo "Alias: sudo iptables" && sudo iptables'
alias rmg='ssh rmg'
alias mg='ssh mg'
alias s='ssh'
alias t='task'

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

function sysr() { sudo systemctl restart $1; journalctl -n 10 -u $1 --no-pager; }
alias sys='sudo systemctl'
alias sysu='systemctl --user'

function wlan() {
    case "$1" in
        on)
            sudo systemctl start netctl-auto@wlan
            ;;
        off)
            sudo systemctl stop netctl-auto@wlan
            sudo resolvconf -d wlan.dhcp
            ;;
        *)
            echo "Usage: wlan {on|off}"
    esac
}

ytplay() (
    init() if     [ "${#1}" -gt 0 ] && i=$? du= f=
then   durl \! \" \# \$ \% \& \' \( \) \* \
            \+ \, \/ \: \; \= \? \@ \[ \]
       : >"${f:=${2:-/tmp/vid}.$(
                      durl "$1" 's/.*mime=[^/]*.\([^&]*\).*/\1/'
                  )}"
       init() { loop; }
    else   ! echo 'NO LINK SPECIFIED!' >&3
fi
    durl() if    [ "${#du}" -eq 0 ]
then  du=$(for c do printf 's/%%%X/\\%s/g;' "'$c" "$c"; done)
    else  curl -s "$1" | { shift
                           sed '/.*url_encoded_fmt_stream_map[^,]*url=/!d
                      s///;s/,.*//;s/\\u0026/\&/g;'"$du$*"; }
fi
    loop() if    [ "$((i=$i+1))" -le 5 ] &&
        sleep "$(($i*2))"
then  play || kill "$pid" || :
    else  ! echo 'ERROR RETRIEVING VIDEO!' >&3
fi
    play() if    [ -s "$f" ]
then  printf '\033}bt%s\0' "$f"; exit
fi
    while init "$@" || exit
    do    curl -s "$(durl "$1")" >"$f" & pid=$!
    done  3>&2 2>/dev/null
)
