#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Xrandr
#if [[ "$(xrandr | grep 'HDMI1 connected')" != "" ]]; then
#  xrandr --output HDMI1 --primary --auto --output LVDS1 --left-of HDMI1
#fi
#if [[ "$(xrandr | grep 'DP3 connected')" != "" ]]; then
#  xrandr --output DP3 --primary --auto --output LVDS1 --left-of HDMI1
#fi

# Set Cursor
xsetroot -cursor_name left_ptr

# Make sure this is before the 'exec' command or it won't be executed.
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile
#
# start xscreensaver
# /usr/bin/xscreensaver -no-splash &
# Set Keyboard layout, moved to /etc/X11/xorg.conf.d/10-keyboard.conf
# setxkbmap -layout de
#run systemd as user instance
# /usr/lib/systemd/systemd --user &
# Start GNOME Keyring
#eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets)
# Export ENV Variables
#export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID
# start xmonad
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
exec xmonad
