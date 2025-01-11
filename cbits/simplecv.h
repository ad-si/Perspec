#pragma once

#include <stdbool.h>

unsigned char const * const apply_gaussian_blur(
  unsigned int width,
  unsigned int height,
  double radius,
  unsigned char const * const data
);

unsigned char const * const grayscale(
  unsigned int width,
  unsigned int height,
  unsigned char const * const data
);

unsigned char const * const grayscale_stretch(
  unsigned int width,
  unsigned int height,
  unsigned char const * const data
);

void apply_global_threshold(
  unsigned int img_length,
  unsigned char * data,
  unsigned char threshold
);

unsigned char const * const otsu_threshold_rgba(
  unsigned int width,
  unsigned int height,
  bool use_double_threshold,
  unsigned char const * const data
);

unsigned char const * const bw_smooth_smart(
  unsigned int width,
  unsigned int height,
  unsigned char const * const data
);
