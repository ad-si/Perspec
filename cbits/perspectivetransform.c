#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "perspectivetransform.h"

// Helper function to solve 8x8 linear system using Gaussian elimination
// Returns 1 on success, 0 on failure
int solve_linear_system(double A[8][8], double b[8], double x[8]) {
  int i, j, k;
  double max, temp, factor;
  const int n = 8;

  // Create augmented matrix [A|b]
  double aug[8][9];
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      aug[i][j] = A[i][j];
    }
    aug[i][n] = b[i];
  }

  // Gaussian elimination with partial pivoting
  for(i = 0; i < n; i++) {
    // Find pivot
    max = fabs(aug[i][i]);
    int max_row = i;
    for(k = i + 1; k < n; k++) {
      if(fabs(aug[k][i]) > max) {
        max = fabs(aug[k][i]);
        max_row = k;
      }
    }

    // Swap maximum row with current row
    if(max_row != i) {
      for(j = i; j <= n; j++) {
        temp = aug[i][j];
        aug[i][j] = aug[max_row][j];
        aug[max_row][j] = temp;
      }
    }

    // Check if pivot is too small
    if(fabs(aug[i][i]) < 1e-10) {
      // Add a small value to avoid singularity
      aug[i][i] += 1e-10;
    }

    // Eliminate column i
    for(j = i + 1; j < n; j++) {
      factor = aug[j][i] / aug[i][i];
      for(k = i; k <= n; k++)
        aug[j][k] -= factor * aug[i][k];
    }
  }

  // Back substitution
  for(i = n - 1; i >= 0; i--) {
    x[i] = aug[i][n];
    for(j = i + 1; j < n; j++)
      x[i] -= aug[i][j] * x[j];
    x[i] /= aug[i][i];
  }

  return 1;
}

// Normalize points to improve numerical stability
static void normalize_points(Point2D points[4], double *scale, double *tx, double *ty) {
  double mean_x = 0, mean_y = 0;
  double sum_dist = 0;
  int i;

  // Calculate centroid
  for(i = 0; i < 4; i++) {
    mean_x += points[i].x;
    mean_y += points[i].y;
  }
  mean_x /= 4;
  mean_y /= 4;

  // Calculate average distance from centroid
  for(i = 0; i < 4; i++) {
    double dx = points[i].x - mean_x;
    double dy = points[i].y - mean_y;
    sum_dist += sqrt(dx*dx + dy*dy);
  }
  double avg_dist = sum_dist / 4;

  // Scale to make average distance from centroid = sqrt(2)
  *scale = (avg_dist > 1e-10) ? sqrt(2.0) / avg_dist : 1.0;
  *tx = -mean_x;
  *ty = -mean_y;

  // Apply normalization transform
  for(i = 0; i < 4; i++) {
    points[i].x = (points[i].x + *tx) * *scale;
    points[i].y = (points[i].y + *ty) * *scale;
  }
}

Matrix3x3 calculate_perspective_transform(Corners src_corners, Corners dst_corners) {
  double A[8][8] = {0};
  double b[8] = {0};
  double x[8] = {0};

  // Set up the system of equations
  for(int i = 0; i < 4; i++) {
    double srcX, srcY, dstX, dstY;
    
    switch(i) {
      case 0: // Top-left
        srcX = src_corners.tl_x; srcY = src_corners.tl_y;
        dstX = dst_corners.tl_x; dstY = dst_corners.tl_y;
        break;
      case 1: // Top-right
        srcX = src_corners.tr_x; srcY = src_corners.tr_y;
        dstX = dst_corners.tr_x; dstY = dst_corners.tr_y;
        break;
      case 2: // Bottom-right
        srcX = src_corners.br_x; srcY = src_corners.br_y;
        dstX = dst_corners.br_x; dstY = dst_corners.br_y;
        break;
      case 3: // Bottom-left
        srcX = src_corners.bl_x; srcY = src_corners.bl_y;
        dstX = dst_corners.bl_x; dstY = dst_corners.bl_y;
        break;
    }

    // First four equations for x coordinates
    A[i][0] = srcX;
    A[i][1] = srcY;
    A[i][2] = 1.0;
    A[i][6] = -srcX * dstX;
    A[i][7] = -srcY * dstX;
    b[i] = dstX;

    // Last four equations for y coordinates
    A[i+4][3] = srcX;
    A[i+4][4] = srcY;
    A[i+4][5] = 1.0;
    A[i+4][6] = -srcX * dstY;
    A[i+4][7] = -srcY * dstY;
    b[i+4] = dstY;
  }

  // Solve the system
  solve_linear_system(A, b, x);

  Matrix3x3 result = {
    x[0], x[1], x[2],
    x[3], x[4], x[5],
    x[6], x[7], 1.0
  };

  return result;
}
