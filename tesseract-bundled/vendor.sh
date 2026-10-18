#!/usr/bin/env bash

# Replace the vendored Tesseract and Leptonica sources with the given versions.
# Afterwards, check the source lists in `tesseract-bundled.cabal`
# against Tesseract's `cmake/SourceLists.cmake`
# and the version in `config/tesseract-include/tesseract/version.h`.
#
# Usage: ./vendor.sh <tesseract-version> <leptonica-version>
# Example: ./vendor.sh 5.5.3 1.87.0

set -euo pipefail

tesseract_version="${1:?Usage: vendor.sh <tesseract-version> <leptonica-version>}"
leptonica_version="${2:?Usage: vendor.sh <tesseract-version> <leptonica-version>}"

package_dir="$(cd "$(dirname "$0")" && pwd)"
download_dir="$(mktemp -d)"
trap 'rm -rf "$download_dir"' EXIT

curl --fail --silent --show-error --location \
  "https://github.com/tesseract-ocr/tesseract/archive/refs/tags/$tesseract_version.tar.gz" \
  | tar -xz -C "$download_dir"
curl --fail --silent --show-error --location \
  "https://github.com/DanBloomberg/leptonica/archive/refs/tags/$leptonica_version.tar.gz" \
  | tar -xz -C "$download_dir"

tesseract_src="$download_dir/tesseract-$tesseract_version"
leptonica_src="$download_dir/leptonica-$leptonica_version"


# Tesseract: Only the library, without the CLI tool and the training tools
rm -rf "$package_dir/tesseract"
mkdir -p "$package_dir/tesseract/src"
cp "$tesseract_src/LICENSE" "$tesseract_src/AUTHORS" "$package_dir/tesseract/"
cp -R "$tesseract_src/include" "$package_dir/tesseract/"
for module in api arch ccmain ccstruct ccutil classify cutil dict lstm textord viewer wordrec
do
  cp -R "$tesseract_src/src/$module" "$package_dir/tesseract/src/"
done


# Leptonica: Only the library, without the programs and regression tests
rm -rf "$package_dir/leptonica"
mkdir -p "$package_dir/leptonica/src"
cp "$leptonica_src/leptonica-license.txt" "$package_dir/leptonica/"
cp "$leptonica_src"/src/*.c "$leptonica_src"/src/*.h "$package_dir/leptonica/src/"


echo "Vendored Tesseract $tesseract_version and Leptonica $leptonica_version"
