/* FlatCV - Amalgamated implementation (auto-generated) */
#define FLATCV_AMALGAMATION
#include "flatcv.h"
#include <assert.h>
#include <math.h>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION          /* normal (non-amalgamated) build */
#include "conversion.h"
#include "perspectivetransform.h"
#else                                /* amalgamated build: already in flatcv.h */
#include "flatcv.h"
#endif

// Avoid using floating point arithmetic by pre-multiplying the weights
const unsigned char R_WEIGHT = 76;  // 0.299 * 256
const unsigned char G_WEIGHT = 150; // 0.587 * 256
const unsigned char B_WEIGHT = 30;  // 0.114 * 256

/**
 * Convert raw RGBA row-major top-to-bottom image data
 * to RGBA row-major top-to-bottom grayscale image data.
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param data Pointer to the pixel data.
 * @return Pointer to the grayscale image data.
 */
unsigned char const * const grayscale(
  unsigned int width,
  unsigned int height,
  unsigned char const * const data
) {
  unsigned int img_length_byte = width * height * 4;
  unsigned char *grayscale_data = malloc(img_length_byte);

  if (!grayscale_data) { // Memory allocation failed
    return NULL;
  }

  // Process each pixel row by row
  for (unsigned int i = 0; i < width * height; i++) {
    unsigned int rgba_index = i * 4;

    unsigned char r = data[rgba_index];
    unsigned char g = data[rgba_index + 1];
    unsigned char b = data[rgba_index + 2];

    unsigned char gray = (r * R_WEIGHT + g * G_WEIGHT + b * B_WEIGHT) >> 8;

    grayscale_data[rgba_index] = gray;
    grayscale_data[rgba_index + 1] = gray;
    grayscale_data[rgba_index + 2] = gray;
    grayscale_data[rgba_index + 3] = 255;
  }

  return grayscale_data;
}


/**
 * Convert raw RGBA row-major top-to-bottom image data
 * to RGBA row-major top-to-bottom grayscale image data
 * with a stretched contrast range.
 * Set the 1.5625 % darkest pixels to 0 and the 1.5625 % brightest to 255.
 * Uses this specific value for speed: x * 1.5625 % = x >> 6
 * The rest of the pixel values are linearly scaled to the range [0, 255].

 * @param width Width of the image.
 * @param height Height of the image.
 * @param data Pointer to the pixel data.
 * @return Pointer to the grayscale image data.
 */
unsigned char const * const grayscale_stretch(
  unsigned int width,
  unsigned int height,
  unsigned char const * const data
) {
  unsigned int img_length_byte = width * height * 4;
  unsigned char *grayscale_data = malloc(img_length_byte);

  if (!grayscale_data) { // Memory allocation failed
    return NULL;
  }

  unsigned int img_length_px = width * height;
  // Ignore 1.5625 % of the pixels
  unsigned int num_pixels_to_ignore = img_length_px >> 6;

  unsigned char *gray_values = malloc(img_length_px);
  if (!gray_values) { // Memory allocation failed
    free(grayscale_data);
    return NULL;
  }

  // Process each pixel row by row to get grayscale values
  for (unsigned int i = 0; i < img_length_px; i++) {
    unsigned int rgba_index = i * 4;

    unsigned char r = data[rgba_index];
    unsigned char g = data[rgba_index + 1];
    unsigned char b = data[rgba_index + 2];

    gray_values[i] = (r * R_WEIGHT + g * G_WEIGHT + b * B_WEIGHT) >> 8;
  }

  // Use counting sort to find the 1.5625% darkest and brightest pixels
  unsigned int histogram[256] = {0};
  for (unsigned int i = 0; i < img_length_px; i++) {
    histogram[gray_values[i]]++;
  }

  unsigned int cumulative_count = 0;
  unsigned char min_val = 0;
  for (unsigned int i = 0; i < 256; i++) {
    cumulative_count += histogram[i];
    if (cumulative_count > num_pixels_to_ignore) {
      min_val = i;
      break;
    }
  }

  cumulative_count = 0;
  unsigned char max_val = 255;
  for (int i = 255; i >= 0; i--) {
    cumulative_count += histogram[i];
    if (cumulative_count > num_pixels_to_ignore) {
      max_val = i;
      break;
    }
  }

  free(gray_values);

  unsigned char range = max_val - min_val;

  // Process each pixel row by row
  for (unsigned int i = 0; i < img_length_px; i++) {
    unsigned int rgba_index = i * 4;

    unsigned char r = data[rgba_index];
    unsigned char g = data[rgba_index + 1];
    unsigned char b = data[rgba_index + 2];

    unsigned char gray = (r * R_WEIGHT + g * G_WEIGHT + b * B_WEIGHT) >> 8;

    if (gray < min_val) {
      gray = 0;
    } else if (gray > max_val) {
      gray = 255;
    } else {
      gray = (gray - min_val) * 255 / range;
    }

    grayscale_data[rgba_index] = gray;
    grayscale_data[rgba_index + 1] = gray;
    grayscale_data[rgba_index + 2] = gray;
    grayscale_data[rgba_index + 3] = 255;
  }

  return grayscale_data;
}


/**
 * Convert raw RGBA row-major top-to-bottom image data
 * to a single channel grayscale image data.
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param data Pointer to the pixel data.
 * @return Pointer to the single channel grayscale image data.
 */
unsigned char *rgba_to_grayscale(
  unsigned int width,
  unsigned int height,
  unsigned char const * const data
) {
  unsigned int img_length_px = width * height;
  unsigned char *grayscale_data = malloc(img_length_px);

  if (!grayscale_data) { // Memory allocation failed
    return NULL;
  }

  // Process each pixel row by row
  for (unsigned int i = 0; i < width * height; i++) {
    unsigned int rgba_index = i * 4;

    unsigned char r = data[rgba_index];
    unsigned char g = data[rgba_index + 1];
    unsigned char b = data[rgba_index + 2];

    unsigned char gray = (r * R_WEIGHT + g * G_WEIGHT + b * B_WEIGHT) >> 8;

    grayscale_data[i] = gray;
  }

  return grayscale_data;
}


/**
  * Apply a global threshold to the image data.
  *
  * @param img_length_px Length of the image data in pixels.
  * @param data Pointer to the image data.
  * @param threshold Threshold value.
  *
  */
void apply_global_threshold(
  unsigned int img_length_px,
  unsigned char *data,
  unsigned char threshold
) {
  for (unsigned int i = 0; i < img_length_px; i++) {
    data[i] = data[i] > threshold ? 255 : 0;
  }
}


/**
  * Applies two thresholds to the image data by blackening pixels
  * below the lower threshold and whitening pixels above the upper threshold.
  * Pixels between the two thresholds are scaled to the range [0, 255].
  *
  * @param img_length_px Length of the image data in pixels.
  * @param data Pointer to the image data.
  * @param lower_threshold Every pixel below this value will be blackened.
  * @param upper_threshold Every pixel above this value will be whitened.
  *
  */
void apply_double_threshold(
  unsigned int img_length_px,
  unsigned char *data,
  unsigned char lower_threshold,
  unsigned char upper_threshold
) {
  for (unsigned int i = 0; i < img_length_px; i++) {
    if (data[i] < lower_threshold) {
      data[i] = 0;
    }
    else if (data[i] > upper_threshold) {
      data[i] = 255;
    }
    else {
      data[i] = (data[i] - lower_threshold) * 255 / (upper_threshold - lower_threshold);
    }
  }
}


/**
  * Convert single channel grayscale image data to
  * RGBA row-major top-to-bottom image data.
  *
  * @param width Width of the image.
  * @param height Height of the image.
  * @param data Pointer to the pixel data.
  */
unsigned char const * const single_to_multichannel(
  unsigned int width,
  unsigned int height,
  unsigned char const * const data
) {
  unsigned int img_length_px = width * height;
  unsigned char *multichannel_data = malloc(img_length_px * 4);

  if (!multichannel_data) { // Memory allocation failed
    return NULL;
  }

  for (unsigned int i = 0; i < img_length_px; i++) {
    unsigned int rgba_index = i * 4;
    multichannel_data[rgba_index] = data[i];
    multichannel_data[rgba_index + 1] = data[i];
    multichannel_data[rgba_index + 2] = data[i];
    multichannel_data[rgba_index + 3] = 255;
  }

  return multichannel_data;
}


/**
  * Apply Otsu's thresholding algorithm to the image data.
  *
  * @param width Width of the image.
  * @param height Height of the image.
  * @param use_double_threshold Whether to use double thresholding.
  * @param data Pointer to the pixel data.
  * @return Pointer to the monochrome image data.
  */
unsigned char const * const otsu_threshold_rgba(
  unsigned int width,
  unsigned int height,
  bool use_double_threshold,
  unsigned char const * const data
) {
  unsigned char *grayscale_img = rgba_to_grayscale(width, height, data);
  unsigned int img_length_px = width * height;

  unsigned int histogram[256] = {0};
  for (unsigned int i = 0; i < img_length_px; i++) {
    histogram[grayscale_img[i]]++;
  }

  float histogram_norm[256] = {0};
  for (unsigned int i = 0; i < 256; i++) {
    histogram_norm[i] = (float)histogram[i] / img_length_px;
  }

  float global_mean = 0.0;

  for (unsigned int i = 0; i < 256; i++) {
    global_mean += i * histogram_norm[i];
  }

  float cumulative_sum = 0.0;
  float cumulative_mean = 0.0;
  float max_variance = 0.0;
  int optimal_threshold = 0;

  for (unsigned int i = 0; i < 256; i++) {
    cumulative_sum += histogram_norm[i];
    cumulative_mean += i * histogram_norm[i];

    if (cumulative_sum == 0 || cumulative_sum == 1) {
      continue;
    }

    float mean1 = cumulative_mean / cumulative_sum;
    float mean2 = (global_mean - cumulative_mean) / (1 - cumulative_sum);

    float class_variance = cumulative_sum * (1 - cumulative_sum) *
                            (mean1 - mean2) * (mean1 - mean2);

    if (class_variance > max_variance) {
      max_variance = class_variance;
      optimal_threshold = i;
    }
  }

  const int threshold_range_offset = 16;

  if (use_double_threshold) {
    apply_double_threshold(
      img_length_px,
      grayscale_img,
      optimal_threshold - threshold_range_offset,
      optimal_threshold + threshold_range_offset
    );
  }
  else {
    apply_global_threshold(
      img_length_px,
      grayscale_img,
      optimal_threshold
    );
  }

  unsigned char const * const monochrome_data = single_to_multichannel(
    width,
    height,
    grayscale_img
  );

  free(grayscale_img);

  return monochrome_data;
}


/**
  * Apply gaussian blur to the image data.
  *
  * @param width Width of the image.
  * @param height Height of the image.
  * @param data Pointer to the pixel data.
  * @return Pointer to the blurred image data.
  */
unsigned char const * const apply_gaussian_blur(
  unsigned int width,
  unsigned int height,
  double radius,
  unsigned char const * const data
) {
  unsigned int img_length_px = width * height;
  if (radius == 0) return memcpy(malloc(width*height*4), data, width*height*4);

  unsigned char *blurred_data = malloc(img_length_px * 4);

  if (!blurred_data) { // Memory allocation failed
    return NULL;
  }

  unsigned int kernel_size = 2 * radius + 1;
  float *kernel = malloc(kernel_size * sizeof(float));

  if (!kernel) { // Memory allocation failed
    free(blurred_data);
    return NULL;
  }

  float sigma = radius / 3.0;
  float sigma_sq = sigma * sigma;
  float two_sigma_sq = 2 * sigma_sq;
  float sqrt_two_pi_sigma = sqrt(2 * M_PI) * sigma;

  for (unsigned int i = 0; i < kernel_size; i++) {
    int x = i - radius;
    kernel[i] = exp(-(x * x) / two_sigma_sq) / sqrt_two_pi_sigma;
  }

  // Apply the kernel in the horizontal direction
  for (unsigned int y = 0; y < height; y++) {
    for (unsigned int x = 0; x < width; x++) {
      float r_sum = 0.0;
      float g_sum = 0.0;
      float b_sum = 0.0;
      float weight_sum = 0.0;

      for (int k = -radius; k <= radius; k++) {
        int x_offset = x + k;
        if (x_offset < 0 || x_offset >= width) {
          continue;
        }

        unsigned int img_index = y * width + x_offset;
        unsigned int img_rgba_index = img_index * 4;

        float weight = kernel[k + (int)radius];
        weight_sum += weight;

        r_sum += data[img_rgba_index] * weight;
        g_sum += data[img_rgba_index + 1] * weight;
        b_sum += data[img_rgba_index + 2] * weight;
      }

      unsigned int rgba_index = (y * width + x) * 4;
      blurred_data[rgba_index] = r_sum / weight_sum;
      blurred_data[rgba_index + 1] = g_sum / weight_sum;
      blurred_data[rgba_index + 2] = b_sum / weight_sum;
      blurred_data[rgba_index + 3] = 255;
    }
  }

  // Apply the kernel in the vertical direction
  for (unsigned int x = 0; x < width; x++) {
    for (unsigned int y = 0; y < height; y++) {
      float r_sum = 0.0;
      float g_sum = 0.0;
      float b_sum = 0.0;
      float weight_sum = 0.0;

      for (int k = -radius; k <= radius; k++) {
        int y_offset = y + k;
        if (y_offset < 0 || y_offset >= height) {
          continue;
        }

        unsigned int img_index = y_offset * width + x;
        unsigned int img_rgba_index = img_index * 4;

        float weight = kernel[k + (int)radius];
        weight_sum += weight;

        r_sum += blurred_data[img_rgba_index] * weight;
        g_sum += blurred_data[img_rgba_index + 1] * weight;
        b_sum += blurred_data[img_rgba_index + 2] * weight;
      }

      unsigned int rgba_index = (y * width + x) * 4;
      blurred_data[rgba_index] = r_sum / weight_sum;
      blurred_data[rgba_index + 1] = g_sum / weight_sum;
      blurred_data[rgba_index + 2] = b_sum / weight_sum;
      blurred_data[rgba_index + 3] = 255;
    }
  }

  free(kernel);

  return blurred_data;
}

#include <time.h>


/**
  * Convert image to anti-aliased black and white.
  * 1. Convert the image to grayscale.
  * 2. Subtract blurred image from the original image to get the high frequencies.
  * 3. Apply OTSU's threshold to get the optimal threshold.
  * 4. Apply the threshold + offset to get the anti-aliased image.
  *
  * @param width Width of the image.
  * @param height Height of the image.
  * @param data Pointer to the pixel data.
  * @return Pointer to the blurred image data.
  */
unsigned char const * const bw_smart(
  unsigned int width,
  unsigned int height,
  bool use_double_threshold,
  unsigned char const * const data
) {
  unsigned char const * const grayscale_data = grayscale(width, height, data);

  // Calculate blur radius dependent on image size
  // (Empirical formula after testing)
  double blurRadius = (sqrt((double)width * (double)height)) * 0.1;

  unsigned char const * const blurred_data = apply_gaussian_blur(
    width,
    height,
    blurRadius,
    grayscale_data
  );

  unsigned int img_length_px = width * height;
  unsigned char *high_freq_data = malloc(img_length_px * 4);

  if (!high_freq_data) { // Memory allocation failed
    free((void *)grayscale_data);
    free((void *)blurred_data);
    return NULL;
  }

  // Subtract blurred image from the original image to get the high frequencies
  // and invert the high frequencies to get a white background.
  for (unsigned int i = 0; i < img_length_px; i++) {
    unsigned int rgba_idx = i * 4;
    int high_freq_val = 127 + grayscale_data[rgba_idx] - blurred_data[rgba_idx];

    // Clamp the value to [0, 255] to prevent overflow
    if (high_freq_val < 0) {
      high_freq_val = 0;
    }
    else if (high_freq_val > 255) {
      high_freq_val = 255;
    }

    high_freq_data[rgba_idx] = high_freq_val; // R
    high_freq_data[rgba_idx + 1] = high_freq_val; // G
    high_freq_data[rgba_idx + 2] = high_freq_val; // B
    high_freq_data[rgba_idx + 3] = 255; // A
  }

  free((void *)grayscale_data);
  free((void *)blurred_data);

  unsigned char const * const final_data = otsu_threshold_rgba(
    width, height, use_double_threshold, high_freq_data
  );

  free(high_freq_data);

  return final_data;
}
#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "perspectivetransform.h"
#else
#include "flatcv.h"
#endif

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

      // Verify that the anchor pixel is inside the source image
      if (x0 >= 0 && x0 < in_width && y0 >= 0 && y0 < in_height) {

        // Clamp the neighbor coordinates so that a (degenerated)
        // bilinear interpolation can be applied at the image borders.
        int x1c = (x1 < in_width)  ? x1 : x0;
        int y1c = (y1 < in_height) ? y1 : y0;

        double dx = srcX - x0;
        double dy = srcY - y0;

        // If a neighbour got clamped we force the corresponding weight to 0
        if (x1c == x0) dx = 0.0;
        if (y1c == y0) dy = 0.0;

        unsigned char *p00 = &in_data[(y0  * in_width + x0 ) * 4];
        unsigned char *p01 = &in_data[(y0  * in_width + x1c) * 4];
        unsigned char *p10 = &in_data[(y1c * in_width + x0 ) * 4];
        unsigned char *p11 = &in_data[(y1c * in_width + x1c) * 4];

        for (int c = 0; c < 4; ++c) {
          out_data[(out_y * out_width + out_x) * 4 + c] = (unsigned char)(
            p00[c] * (1 - dx) * (1 - dy) +
            p01[c] * dx       * (1 - dy) +
            p10[c] * (1 - dx) * dy       +
            p11[c] * dx       * dy
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
/* End of FlatCV amalgamation */
