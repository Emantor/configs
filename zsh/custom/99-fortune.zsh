# Echo fortune on new terminal
if [ -e `which fortune` ]; then
  if [ -e /usr/share/fortune/discworld ]; then
    echo "`fortune discworld`"
  else
    echo "`fortune`"
  fi
fi
