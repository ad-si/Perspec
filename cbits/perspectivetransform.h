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
