/*
 * Replacement for the `endianness.h` that Leptonica's build systems generate
 * from `endianness.h.in`.
 * GCC and Clang define the byte order of the target.
 */

#if !defined(L_BIG_ENDIAN) && !defined(L_LITTLE_ENDIAN)
#  if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#    define L_BIG_ENDIAN
#  else
#    define L_LITTLE_ENDIAN
#  endif
#endif
