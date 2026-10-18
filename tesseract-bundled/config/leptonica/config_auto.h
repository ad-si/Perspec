/*
 * Replacement for the `config_auto.h` that Leptonica's build systems generate.
 *
 * Tesseract only passes raw pixels to Leptonica,
 * so it's built without any image format libraries.
 * Leptonica then compiles stubs for the reading and writing functions,
 * which return an error.
 */

#define HAVE_LIBGIF 0
#define HAVE_LIBJP2K 0
#define HAVE_LIBJPEG 0
#define HAVE_LIBPNG 0
#define HAVE_LIBTIFF 0
#define HAVE_LIBUNGIF 0
#define HAVE_LIBWEBP 0
#define HAVE_LIBWEBP_ANIM 0
#define HAVE_LIBZ 0

/* Not available on Windows, so don't depend on them anywhere */
#define HAVE_FMEMOPEN 0
#define HAVE_FSTATAT 0
#define HAVE_DIRFD 0
