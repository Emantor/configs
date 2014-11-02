#!/bin/zsh
host=78.46.234.214

while true; do
ping=$(ping -c 1 -W 5 $host 2> /dev/null | grep 'icmp_seq' | cut -d '=' -f4)
if [[ $ping == '' ]]; then
  echo "<fc=red>D</fc>" > /home/phoenix/.xmonad/inettest
else
  echo "<fc=green>$ping</fc>" > /home/phoenix/.xmonad/inettest

fi
sleep 5;
done;
