#! /usr/bin/env sh

if test $# == 0
then

# Lines must contain at least one space or they are discarded
printf " "
printf "
                                     USAGE
────────────────────────────────────────────────────────────────────────────────
 "
printf "
1. Drop the image file(s) onto this window
2. Mark the corners by clicking on them
3. Press [Enter]
 "
printf "
You can also use it directly via the command line.
 "
printf "
For more information open your terminal and run \`perspec help\`.
(On macOS \`/Applications/Perspec.app/Contents/Resources/perspec help\`.)
 "
printf "
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
 "

else
  ./perspec fix "$@"
fi
