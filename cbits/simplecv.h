#pragma once

unsigned char const * const grayscale(
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
  unsigned char const * const data
);
