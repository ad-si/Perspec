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
    for(j = 0; j < n; j++)
      aug[i][j] = A[i][j];
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

    // Check for singular matrix
    if(fabs(aug[i][i]) < 1e-10)
      return 0;

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

Matrix3x3 calculate_perspective_transform(Corners src_corners, Corners dst_corners) {
  Point2D src[4] = {
    {src_corners.tl_x, src_corners.tl_y},
    {src_corners.tr_x, src_corners.tr_y},
    {src_corners.br_x, src_corners.br_y},
    {src_corners.bl_x, src_corners.bl_y}
  };
  Point2D dst[4] = {
    {dst_corners.tl_x, dst_corners.tl_y},
    {dst_corners.tr_x, dst_corners.tr_y},
    {dst_corners.br_x, dst_corners.br_y},
    {dst_corners.bl_x, dst_corners.bl_y}
  };

  double A[8][8] = {0};
  double b[8] = {0};
  double x[8] = {0};

  // Build matrix A and vector b as per the mathematical formulation
  for(int i = 0; i < 4; i++) {
    // First four rows
    A[i][0] = src[i].x;
    A[i][1] = src[i].y;
    A[i][2] = 1;
    A[i][6] = -src[i].x * dst[i].x;
    A[i][7] = -src[i].y * dst[i].x;
    b[i] = dst[i].x;

    // Last four rows
    A[i+4][3] = src[i].x;
    A[i+4][4] = src[i].y;
    A[i+4][5] = 1;
    A[i+4][6] = -src[i].x * dst[i].y;
    A[i+4][7] = -src[i].y * dst[i].y;
    b[i+4] = dst[i].y;
  }

  // Solve the system
  if (!solve_linear_system(A, b, x)) { // Handle error case
    Matrix3x3 identityMatrix = {
      1.0, 1.0, 1.0,
      1.0, 1.0, 1.0,
      1.0, 1.0, 1.0
    };
    return identityMatrix;
  }

  // Build the result matrix
  Matrix3x3 result = {
    x[0],
    x[1],
    x[2],
    x[3],
    x[4],
    x[5],
    x[6],
    x[7],
    1.0  // Last element is always 1
  };

  return result;
}


// Test the perspective transform calculation
int main() {
  Corners src = {
    0, 0,  // Top-left
    1, 0,  // Top-right
    1, 1,  // Bottom-right
    0, 1   // Bottom-left
  };

  Corners dst = {
    0, 0,  // Top-left
    2, 0,  // Top-right
    2, 2,  // Bottom-right
    0, 2   // Bottom-left
  };

  Matrix3x3 trans_mat = calculate_perspective_transform(src, dst);

  const double eps0 = 1e-10;

  assert(fabs(trans_mat.m00 - 2) < eps0);
  assert(fabs(trans_mat.m01) < eps0);
  assert(fabs(trans_mat.m02) < eps0);
  assert(fabs(trans_mat.m10) < eps0);
  assert(fabs(trans_mat.m11 - 2) < eps0);
  assert(fabs(trans_mat.m12) < eps0);
  assert(fabs(trans_mat.m20) < eps0);
  assert(fabs(trans_mat.m21) < eps0);
  assert(fabs(trans_mat.m22 - 1) < eps0);

  printf("✅ Test passed\n");
}
