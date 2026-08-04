#!/usr/bin/env bash

# Copy every non-system dynamic library that the `perspec` binary depends on
# into the app bundle and rewrite the install names to the bundled copies.
#
# Without this, the app links against libraries by absolute path
# (e.g. `/opt/homebrew/opt/webp/lib/libwebp.7.dylib`)
# and therefore only launches on machines
# that have the very same libraries at the very same paths.

set -euo pipefail

app_bundle="${1:?Usage: bundle-dylibs.sh <path-to-Perspec.app>}"
binary="$app_bundle/Contents/Resources/perspec"
lib_dir="$app_bundle/Contents/Frameworks"

if ! test -f "$binary"
then
  echo "bundle-dylibs: no binary at $binary" >&2
  exit 1
fi


# Libraries that are guaranteed to be present on every macOS installation
is_system_lib() {
  case "$1" in
    /usr/lib/* | /System/*) return 0 ;;
    *) return 1 ;;
  esac
}


# All libraries $1 links against, excluding its own install name
dependencies_of() {
  local file="$1"
  local own_id
  own_id="$(otool -D "$file" | awk 'NR > 1')"
  otool -L "$file" \
    | awk 'NR > 1 { sub(/ \(compatibility version.*/, ""); \
                    sub(/^[ \t]+/, ""); print }' \
    | awk -v own="$own_id" 'length($0) > 0 && $0 != own'
}


# The `LC_RPATH` entries of $1
rpaths_of() {
  otool -l "$1" \
    | awk '$2 == "LC_RPATH" { is_rpath = 1 } \
           is_rpath && $1 == "path" { print $2; is_rpath = 0 }'
}


# Turn a (possibly `@rpath` relative) install name into an absolute path.
# $2 is the file that links against it,
# $3 the directory it was originally loaded from
# (which differs from $2 once the library has been copied into the bundle).
resolve_dependency() {
  local dep="$1"
  local referrer="$2"
  local referrer_dir="$3"
  local candidate
  local rpath

  case "$dep" in
    @rpath/*)
      for rpath in $(rpaths_of "$referrer")
      do
        candidate="$rpath/${dep#@rpath/}"
        candidate="${candidate//@loader_path/$referrer_dir}"
        candidate="${candidate//@executable_path/$referrer_dir}"
        if test -f "$candidate"
        then
          printf '%s\n' "$candidate"
          return 0
        fi
      done
      return 1
      ;;
    @loader_path/* | @executable_path/*)
      candidate="${dep//@loader_path/$referrer_dir}"
      candidate="${candidate//@executable_path/$referrer_dir}"
      test -f "$candidate" || return 1
      printf '%s\n' "$candidate"
      ;;
    *)
      test -f "$dep" || return 1
      printf '%s\n' "$dep"
      ;;
  esac
}


mkdir -p "$lib_dir"

# Breadth first walk over the binary and all bundled libraries.
# New entries are appended while the loop is running.
# `origins` holds the directory each file was copied from,
# because `@rpath` and `@loader_path` must be resolved
# relative to the original location, not the one inside the bundle.
files=("$binary")
origins=("$(cd "$(dirname "$binary")" && pwd)")
index=0

while test "$index" -lt "${#files[@]}"
do
  file="${files[$index]}"
  origin_dir="${origins[$index]}"
  index=$((index + 1))

  # Libraries live next to each other in `Contents/Frameworks`,
  # the binary lives one directory below in `Contents/Resources`.
  if test "$file" = "$binary"
  then link_prefix='@loader_path/../Frameworks'
  else link_prefix='@loader_path'
  fi

  dependencies="$(dependencies_of "$file")"

  while IFS= read -r dep
  do
    test -n "$dep" || continue
    is_system_lib "$dep" && continue

    if ! resolved="$(resolve_dependency "$dep" "$file" "$origin_dir")"
    then
      echo "bundle-dylibs: cannot resolve \"$dep\" of \"$file\"" >&2
      exit 1
    fi

    lib_name="$(basename "$resolved")"

    if ! test -f "$lib_dir/$lib_name"
    then
      cp "$resolved" "$lib_dir/$lib_name"
      chmod u+w "$lib_dir/$lib_name"
      install_name_tool -id "@loader_path/$lib_name" "$lib_dir/$lib_name" \
        2> /dev/null
      files+=("$lib_dir/$lib_name")
      origins+=("$(cd "$(dirname "$resolved")" && pwd)")
      echo "bundle-dylibs: bundled $lib_name"
    fi

    # Warnings about the invalidated code signature are expected,
    # as everything gets signed again further below.
    install_name_tool -change "$dep" "$link_prefix/$lib_name" "$file" \
      2> /dev/null
  done <<< "$dependencies"
done


# `install_name_tool` invalidates the code signature,
# which makes the binary unlaunchable on Apple Silicon.
# Sign the nested libraries before the binary that loads them.
for ((index = ${#files[@]} - 1; index >= 0; index--))
do
  codesign --force --sign - "${files[$index]}" 2> /dev/null
done


# Fail the build instead of shipping an app that crashes on launch
for file in "${files[@]}"
do
  while IFS= read -r dep
  do
    test -n "$dep" || continue
    is_system_lib "$dep" && continue
    case "$dep" in
      @loader_path/*) continue ;;
    esac
    echo "bundle-dylibs: \"$file\" still links against \"$dep\"" >&2
    exit 1
  done <<< "$(dependencies_of "$file")"
done

echo "bundle-dylibs: $((${#files[@]} - 1)) libraries bundled into $lib_dir"
