# tesseract-bundled

Compiles [Tesseract] and [Leptonica] from source as part of the Haskell build,
so Perspec can use Tesseract's C API (`tesseract/capi.h`)
without a system installation and without shipping any shared libraries.

[Tesseract]: https://github.com/tesseract-ocr/tesseract
[Leptonica]: https://github.com/DanBloomberg/leptonica

The package doesn't contain any Haskell modules.
Declare the needed functions with `foreign import ccall`.


## Build Configuration

The upstream build systems generate some headers,
which are replaced by hand-written versions in `config/`:

- `tesseract/config_auto.h`: Disables the legacy engine
    and the ScrollView debugging window.
- `tesseract-include/tesseract/version.h`
- `leptonica/config_auto.h`: Disables all image format libraries
    (Leptonica then compiles stubs that return an error),
    and functions that aren't available on every platform.
- `leptonica-include/endianness.h`

The source lists in `tesseract-bundled.cabal` mirror
Tesseract's `cmake/SourceLists.cmake` without `TESSERACT_SRC_LEGACY`.


## Updating

```sh
./vendor.sh <tesseract-version> <leptonica-version>
```

Then update:

- The source lists in `tesseract-bundled.cabal`,
    in case files were added or removed upstream
- The versions in `tesseract-bundled.cabal`,
    `config/tesseract/config_auto.h`,
    and `config/tesseract-include/tesseract/version.h`
