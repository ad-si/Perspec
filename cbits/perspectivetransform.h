#ifndef PERSPECTIVE_TRANSFORM_H
#define PERSPECTIVE_TRANSFORM_H

typedef struct {
  double x;
  double y;
} Point2D;

typedef struct {
  double tl_x;
  double tl_y;
  double tr_x;
  double tr_y;
  double br_x;
  double br_y;
  double bl_x;
  double bl_y;
} Corners;

typedef struct {
  double m00;
  double m01;
  double m02;
  double m10;
  double m11;
  double m12;
  double m20;
  double m21;
  double m22;
} Matrix3x3;

Matrix3x3* calculate_perspective_transform(
  Corners* src_corners,
  Corners* dst_corners
);

#endif
