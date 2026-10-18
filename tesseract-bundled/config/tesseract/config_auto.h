/*
 * Replacement for the `config_auto.h` that Tesseract's build systems generate.
 * Only the options checked by the library sources are defined.
 * The CPU specific options (HAVE_AVX, HAVE_SSE4_1, …) are set
 * in `tesseract-bundled.cabal`, as they depend on the architecture.
 */

#define PACKAGE_VERSION "5.5.3"

/* Only the LSTM engine is used, the fast models don't support the legacy one */
#define DISABLED_LEGACY_ENGINE 1

/* No ScrollView debugging window */
#define GRAPHICS_DISABLED 1

/* Use float instead of double for the LSTM, like Tesseract's default build */
#define FAST_FLOAT 1
