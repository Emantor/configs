# Load zsh files. These are loaded in order (01-99), local overwrites custom.
for config_file in $ZSH/(local|custom)/*.zsh(Noe!'REPLY=${REPLY:t}'!oe!'[[ $REPLY == *local* ]] && REPLY=0 || REPLY=1'!); source $config_file

PATH="/home/phoenix/perl5/bin${PATH+:}${PATH}"; export PATH;
PERL5LIB="/home/phoenix/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/phoenix/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/phoenix/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/phoenix/perl5"; export PERL_MM_OPT;
