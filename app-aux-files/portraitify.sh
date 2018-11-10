#! /usr/bin/env bash

find "$1" -iname '*.jpg' -or -iname '*.jpeg' | \
while read -r file
do
  mogrify "$file" -rotate "-90>"
done
