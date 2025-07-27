/* FlatCV - Amalgamated public header (auto-generated) */
#ifndef FLATCV_H
#define FLATCV_H
#pragma once

typedef struct {
  double x;
  double y;
} Point2D;

typedef struct {
  double tl_x, tl_y;
  double tr_x, tr_y;
  double br_x, br_y;
  double bl_x, bl_y;
} Corners;

typedef struct {
  double m00, m01, m02;
  double m10, m11, m12;
  double m20, m21, m22;
} Matrix3x3;

Matrix3x3* calculate_perspective_transform(
  Corners* src_corners,
  Corners* dst_corners
);

unsigned char * apply_matrix_3x3(
  int in_width,
  int in_height,
  unsigned char* in_data,
  int out_width,
  int out_height,
  Matrix3x3* tmat
);
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

unsigned char const * const bw_smart(
  unsigned int width,
  unsigned int height,
  bool use_double_threshold,
  unsigned char const * const data
);
#endif /* FLATCV_H */
