#!/usr/bin/env bash

# Replaces `licenses = []` in source/Lib.hs with the keys in $LICENSE_KEYS.
# Keys can be separated by commas and/or newlines
# and can optionally be wrapped in double quotes.
# Fails if no key was found or if the placeholder wasn't replaced.

set -euo pipefail

sourceFile="source/Lib.hs"

if test -z "${LICENSE_KEYS:-}"
then
  echo "::error::LICENSE_KEYS is empty"
  exit 1
fi

licensesList="$(
  printf '%s\n' "$LICENSE_KEYS" \
    | tr ',\r' '\n\n' \
    | awk '
        {
          gsub(/^[[:space:]]+|[[:space:]]+$/, "")
          gsub(/^"|"$/, "")
          if ($0 == "") next
          gsub(/\\/, "\\\\")
          gsub(/"/, "\\\"")
          keys = keys (keys == "" ? "" : ", ") "\"" $0 "\""
          count++
        }
        END {
          if (count == 0) exit 1
          print "licenses = [" keys "]"
        }
      '
)" || {
  echo "::error::LICENSE_KEYS doesn't contain any keys"
  exit 1
}

# Windows checkouts can have CRLF line endings
if ! LICENSES_LIST="$licensesList" awk '
  /^licenses = \[\]\r?$/ {
    print ENVIRON["LICENSES_LIST"] (/\r$/ ? "\r" : "")
    found = 1
    next
  }
  { print }
  END { if (!found) exit 1 }
' "$sourceFile" > "$sourceFile.tmp"
then
  rm -f "$sourceFile.tmp"
  echo "::error::Placeholder 'licenses = []' not found in $sourceFile"
  exit 1
fi
mv "$sourceFile.tmp" "$sourceFile"

keyCount="$(printf '%s\n' "$licensesList" | awk -F '", "' '{ print NF }')"
echo "Injected $keyCount license keys into $sourceFile"
