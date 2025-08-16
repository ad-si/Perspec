/* FlatCV - Amalgamated public header (auto-generated) */
/*
 * ISC License
 * 
 * Copyright (c) 2025 Adrian Sieber
 * 
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef FLATCV_H
#define FLATCV_H
#define FLATCV_AMALGAMATION
// File: include/1_types.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdint.h>

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
// File: include/binary_closing_disk.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdint.h>

uint8_t *fcv_binary_closing_disk(
  uint8_t const *image_data,
  int32_t width,
  int32_t height,
  int32_t radius
);
// File: include/conversion.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdbool.h>
#include <stdint.h>

uint8_t *fcv_apply_gaussian_blur(
  uint32_t width,
  uint32_t height,
  double radius,
  uint8_t const * const data
);

uint8_t *fcv_grayscale(
  uint32_t width,
  uint32_t height,
  uint8_t const * const data
);

uint8_t *fcv_grayscale_stretch(
  uint32_t width,
  uint32_t height,
  uint8_t const * const data
);

void fcv_apply_global_threshold(
  uint32_t img_length,
  uint8_t *data,
  uint8_t threshold
);

uint8_t *fcv_otsu_threshold_rgba(
  uint32_t width,
  uint32_t height,
  bool use_double_threshold,
  uint8_t const * const data
);

uint8_t *fcv_bw_smart(
  uint32_t width,
  uint32_t height,
  bool use_double_threshold,
  uint8_t const * const data
);

uint8_t *fcv_resize(
  uint32_t width,
  uint32_t height,
  double scale_x,
  double scale_y,
  uint32_t* out_width,
  uint32_t* out_height,
  uint8_t const * const data
);
// File: include/convert_to_binary.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdint.h>

uint8_t *fcv_convert_to_binary(
  const uint8_t *image_data,
  int32_t width,
  int32_t height,
  const char *foreground_hex,
  const char *background_hex
);
// File: include/corner_peaks.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#include "1_types.h"
#endif

typedef struct {
  Point2D *points;
  uint32_t count;
} CornerPeaks;

CornerPeaks *fcv_corner_peaks(
  uint32_t width,
  uint32_t height,
  uint8_t const *data,
  uint32_t min_distance,
  double accuracy_thresh,
  double roundness_thresh
);
// File: include/crop.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdint.h>

uint8_t *fcv_crop(
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  uint8_t const * const data,
  uint32_t x,
  uint32_t y,
  uint32_t new_width,
  uint32_t new_height
);
// File: include/draw.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdint.h>

void fcv_set_circle_pixel(
  uint8_t *data,
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  int32_t px,
  int32_t py,
  uint8_t r,
  uint8_t g,
  uint8_t b
);

void fcv_draw_circle(
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  const char *hex_color,
  double radius,
  double center_x,
  double center_y,
  uint8_t *data
);

void fcv_draw_disk(
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  const char *hex_color,
  double radius,
  double center_x,
  double center_y,
  uint8_t *data
);

void fcv_draw_circle_points(
  uint8_t *data,
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  int32_t cx,
  int32_t cy,
  int32_t x,
  int32_t y,
  uint8_t r,
  uint8_t g,
  uint8_t b
);

void fcv_fill_disk_lines(
  uint8_t *data,
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  int32_t cx,
  int32_t cy,
  int32_t x,
  int32_t y,
  uint8_t r,
  uint8_t g,
  uint8_t b
);
// File: include/foerstner_corner.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdint.h>

uint8_t *fcv_foerstner_corner(
  uint32_t width,
  uint32_t height,
  uint8_t const * const data,
  double sigma
);
// File: include/parse_hex_color.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdint.h>

void fcv_parse_hex_color(
  const char *hex_color,
  uint8_t *r,
  uint8_t *g,
  uint8_t *b
);
// File: include/perspectivetransform.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#include "1_types.h"
#endif

Matrix3x3 *fcv_calculate_perspective_transform(
  Corners *src_corners,
  Corners *dst_corners
);

uint8_t *fcv_apply_matrix_3x3(
  int32_t in_width,
  int32_t in_height,
  uint8_t *in_data,
  int32_t out_width,
  int32_t out_height,
  Matrix3x3 *tmat
);

Corners fcv_detect_corners(const uint8_t *image, int32_t width, int32_t height);
// File: include/rgba_to_grayscale.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdint.h>

// Avoid using floating point arithmetic by pre-multiplying the weights
#define R_WEIGHT 76  // 0.299 * 256
#define G_WEIGHT 150 // 0.587 * 256
#define B_WEIGHT 30  // 0.114 * 256

uint8_t *fcv_rgba_to_grayscale(
  uint32_t width,
  uint32_t height,
  uint8_t const * const data
);
// File: include/single_to_multichannel.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdint.h>

uint8_t *fcv_single_to_multichannel(
  uint32_t width,
  uint32_t height,
  uint8_t const *const data
);
// File: include/sobel_edge_detection.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#endif

#include <stdint.h>

uint8_t *fcv_sobel_edge_detection(
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  uint8_t const * const data
);
// File: include/watershed_segmentation.h
#ifndef FLATCV_AMALGAMATION
#pragma once
#include "1_types.h"
#endif

#include <stdint.h>
#include <stdbool.h>

uint8_t *fcv_watershed_segmentation(
  uint32_t width,
  uint32_t height,
  uint8_t const * const grayscale_data,
  Point2D* markers,
  uint32_t num_markers,
  bool create_boundaries
);
#endif /* FLATCV_H */
