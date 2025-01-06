#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "perspectivetransform.h"

// #define DEBUG_LOGGING

#ifdef DEBUG_LOGGING
  #define log(msg) printf("DEBUG: %s\n", msg)
#else
  #define log(msg) // No operation
#endif


/**
  * Helper function to solve 8x8 linear system using Gaussian elimination
  * Returns 1 on success, 0 on failure
  */
int solve_linear_system(double A[8][8], double b[8], double x[8]) {
  const int n = 8;
  const double epsilon = 1e-10;
  int i, j, k;

  // Create augmented matrix [A|b] with extra safety margin
  double aug[8][10];  // One extra column for safety

  // Initialize augmented matrix
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      aug[i][j] = A[i][j];
    }
    aug[i][n] = b[i];
  }

  // Gaussian elimination with partial pivoting
  for(i = 0; i < n; i++) {
    // Find pivot
    int max_row = i;
    double max_val = fabs(aug[i][i]);

    for(k = i + 1; k < n; k++) {
      if(fabs(aug[k][i]) > max_val) {
        max_val = fabs(aug[k][i]);
        max_row = k;
      }
    }

    // Check for singularity
    if(max_val < epsilon) {
      log("Warning: Matrix is nearly singular\n");
      return 0;
    }

    // Swap maximum row with current row
    if(max_row != i) {
      for(j = 0; j <= n; j++) {
        double temp = aug[i][j];
        aug[i][j] = aug[max_row][j];
        aug[max_row][j] = temp;
      }
    }

    // Eliminate column i
    for(j = i + 1; j < n; j++) {
      double factor = aug[j][i] / aug[i][i];
      for(k = i; k <= n; k++) {
        aug[j][k] -= factor * aug[i][k];
      }
    }
  }

  // Back substitution
  for(i = n - 1; i >= 0; i--) {
    if(fabs(aug[i][i]) < epsilon) {
      log("Warning: Zero pivot encountered\n");
      return 0;
    }

    x[i] = aug[i][n];
    for(j = i + 1; j < n; j++) {
      x[i] -= aug[i][j] * x[j];
    }
    x[i] /= aug[i][i];

    // Check for invalid results
    if(isnan(x[i]) || isinf(x[i])) {
      log("Warning: Invalid result detected\n");
      return 0;
    }
  }

  return 1;
}


/**
  * Calculate the perspective transformation matrix
  * from the source and destination corner coordinates.
  */
Matrix3x3 *calculate_perspective_transform(
  Corners *src_corners,
  Corners *dst_corners
) {
  // Initialize matrices with zeros
  double A[8][8] = {{0}};
  double b[8] = {0};
  double x[8] = {0};

  // Identity matrix as fallback
  static Matrix3x3 identity = {
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    0.0, 0.0, 1.0
  };

  if (!src_corners || !dst_corners) {
    log("Error: NULL pointer passed to calculate_perspective_transform\n");
    return &identity;
  }

  #ifdef DEBUG_LOGGING
    printf("[C] Calculating perspective transform:\n");
    printf("src_corners:\ntl(%f, %f)\ntr(%f, %f)\nbr(%f, %f)\nbl(%f, %f)\n\n",
      src_corners->tl_x, src_corners->tl_y,
      src_corners->tr_x, src_corners->tr_y,
      src_corners->br_x, src_corners->br_y,
      src_corners->bl_x, src_corners->bl_y
    );
    printf("dst_corners:\ntl(%f, %f)\ntr(%f, %f)\nbr(%f, %f)\nbl(%f, %f)\n\n",
      dst_corners->tl_x, dst_corners->tl_y,
      dst_corners->tr_x, dst_corners->tr_y,
      dst_corners->br_x, dst_corners->br_y,
      dst_corners->bl_x, dst_corners->bl_y
    );
  #endif

  // Validate input coordinates
  if (
    isnan(src_corners->tl_x) || isnan(src_corners->tl_y) ||
    isnan(src_corners->tr_x) || isnan(src_corners->tr_y) ||
    isnan(src_corners->br_x) || isnan(src_corners->br_y) ||
    isnan(src_corners->bl_x) || isnan(src_corners->bl_y) ||
    isnan(dst_corners->tl_x) || isnan(dst_corners->tl_y) ||
    isnan(dst_corners->tr_x) || isnan(dst_corners->tr_y) ||
    isnan(dst_corners->br_x) || isnan(dst_corners->br_y) ||
    isnan(dst_corners->bl_x) || isnan(dst_corners->bl_y)
  ) {
    log("Error: Invalid coordinates (NaN) detected\n");
    return &identity;
  }

  // Set up the system of equations
  for(int i = 0; i < 4; i++) {
    double srcX = 0.0, srcY = 0.0, dstX = 0.0, dstY = 0.0;

    // Safely extract coordinates
    switch(i) {
      case 0: // Top-left
        srcX = src_corners->tl_x; srcY = src_corners->tl_y;
        dstX = dst_corners->tl_x; dstY = dst_corners->tl_y;
        break;
      case 1: // Top-right
        srcX = src_corners->tr_x; srcY = src_corners->tr_y;
        dstX = dst_corners->tr_x; dstY = dst_corners->tr_y;
        break;
      case 2: // Bottom-right
        srcX = src_corners->br_x; srcY = src_corners->br_y;
        dstX = dst_corners->br_x; dstY = dst_corners->br_y;
        break;
      case 3: // Bottom-left
        srcX = src_corners->bl_x; srcY = src_corners->bl_y;
        dstX = dst_corners->bl_x; dstY = dst_corners->bl_y;
        break;
    }

    // Validate extracted coordinates
    if (isinf(srcX) || isinf(srcY) || isinf(dstX) || isinf(dstY)) {
      log("Error: Invalid coordinates (Inf) detected\n");
      return &identity;
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

  log("Solve the system of equations …\n");
  if (!solve_linear_system(A, b, x)) {
    log("Failed to solve system, returning identity matrix\n");
    return &identity;
  }

  // Validate solution
  for (int i = 0; i < 8; i++) {
    if (isnan(x[i]) || isinf(x[i]) || fabs(x[i]) > 1e6) {
      log("Error: Invalid solution values detected\n");
      return &identity;
    }
  }

  Matrix3x3* result = malloc(sizeof(Matrix3x3));
  *result = (Matrix3x3) {
    x[0], x[1], x[2],
    x[3], x[4], x[5],
    x[6], x[7], 1.0
  };

  #ifdef DEBUG_LOGGING
    printf("Result matrix:\n");
    printf("%f, %f, %f\n", result->m00, result->m01, result->m02);
    printf("%f, %f, %f\n", result->m10, result->m11, result->m12);
    printf("%f, %f, %f\n", result->m20, result->m21, result->m22);
  #endif

  // Final validation of the result matrix
  if (
    isnan(result->m00) || isnan(result->m01) || isnan(result->m02) ||
    isnan(result->m10) || isnan(result->m11) || isnan(result->m12) ||
    isnan(result->m20) || isnan(result->m21) || isnan(result->m22) ||
    isinf(result->m00) || isinf(result->m01) || isinf(result->m02) ||
    isinf(result->m10) || isinf(result->m11) || isinf(result->m12) ||
    isinf(result->m20) || isinf(result->m21) || isinf(result->m22)
  ) {
    log("Error: Invalid values in result matrix\n");
    return &identity;
  }

  return result;
}


/**
  * Apply the transformation matrix to the input image
  * and store the result in the output image.
  * Use bilinear interpolation to calculate final pixel values.
  */
unsigned char *apply_matrix_3x3(
  int in_width,
  int in_height,
  unsigned char* in_data,
  int out_width,
  int out_height,
  Matrix3x3* tmat
) {
  #ifdef DEBUG_LOGGING
    printf("Input data:\n");
    for (int i = 0; i < in_width; i++) {
      for (int j = 0; j < in_height; j++) {
        printf("%d ", in_data[(i * in_width + j) * 4]);
      }
      printf("\n");
    }
  #endif

  // Patch flip matrix if needed
  if (
    fabs(tmat->m00 + 1.0) < 1e-9 &&
    fabs(tmat->m11 + 1.0) < 1e-9 &&
    tmat->m02 == 0.0 &&
    tmat->m12 == 0.0
  ) {
    tmat->m02 = in_width - 1;
    tmat->m12 = in_height - 1;
  }

  unsigned char *out_data = calloc(
    out_width * out_height * 4,
    sizeof(unsigned char)
  );

  if (!out_data) { // Memory allocation failed
    return NULL;
  }

  // Iterate through every pixel in the output image
  for (int out_y = 0; out_y < out_height; ++out_y) {
    for (int out_x = 0; out_x < out_width; ++out_x) {
      // Apply the inverse transformation to find the corresponding source pixel
      double w = tmat->m20 * out_x + tmat->m21 * out_y + tmat->m22;
      if (fabs(w) < 1e-10) continue;  // Skip if w is too close to zero

      double srcX = (tmat->m00 * out_x + tmat->m01 * out_y + tmat->m02) / w;
      double srcY = (tmat->m10 * out_x + tmat->m11 * out_y + tmat->m12) / w;

      // Convert source coordinates to integers
      int x0 = (int)floor(srcX);
      int y0 = (int)floor(srcY);
      int x1 = x0 + 1;
      int y1 = y0 + 1;

      // Check if the source coordinates are within bounds
      if (x0 >= 0 && x0 < in_width && y0 >= 0 && y0 < in_height &&
          x1 >= 0 && x1 < in_width && y1 >= 0 && y1 < in_height) {
        // Calculate the weights for bilinear interpolation
        double dx = srcX - x0;
        double dy = srcY - y0;

        // Get the four surrounding pixels
        unsigned char *p00 = &in_data[(y0 * in_width + x0) * 4];
        unsigned char *p01 = &in_data[(y0 * in_width + x1) * 4];
        unsigned char *p10 = &in_data[(y1 * in_width + x0) * 4];
        unsigned char *p11 = &in_data[(y1 * in_width + x1) * 4];

        // Interpolate the pixel values
        for (int c = 0; c < 4; ++c) {
          out_data[(out_y * out_width + out_x) * 4 + c] = (unsigned char)(
            p00[c] * (1 - dx) * (1 - dy) +
            p01[c] * dx * (1 - dy) +
            p10[c] * (1 - dx) * dy +
            p11[c] * dx * dy
          );
        }
      }
    }
  }

  #ifdef DEBUG_LOGGING
    printf("Output data:\n");
    for (int i = 0; i < out_width; i++) {
      for (int j = 0; j < out_height; j++) {
        printf("%d ", out_data[(i * out_width + j) * 4]);
      }
      printf("\n");
    }
  #endif

  return out_data;
}
