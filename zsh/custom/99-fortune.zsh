# Echo fortune on new terminal
fort_path=`which fortune`
if [ -e $fort_path ]; then
  if [ -e /usr/share/fortune/discworld ]; then
    echo "`fortune discworld`"
  else
    echo "`fortune`"
  fi
fi
