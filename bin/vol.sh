#/bin/bash
vol=$(amixer get Master | awk -F'[]%[]' '/%/ {if ($5 == "off") { print "MM" } else { print $2 }}' | head -n 1)

echo $vol%
exit 0
