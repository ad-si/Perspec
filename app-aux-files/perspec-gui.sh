#! /bin/dash

if test $# = 0
then ./perspec gui
else ./perspec fix "$@"
fi
