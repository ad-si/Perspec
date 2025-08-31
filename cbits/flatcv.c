/* FlatCV - Amalgamated implementation (auto-generated) */
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

#define FLATCV_AMALGAMATION
#include "flatcv.h"
// File: src/binary_closing_disk.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef FLATCV_AMALGAMATION
#include "conversion.h"
#include "perspectivetransform.h"
#else
#include "flatcv.h"
#endif

static uint8_t *binary_dilation_disk(
  uint8_t const *image_data,
  int32_t width,
  int32_t height,
  int32_t radius
) {
  if (!image_data || width <= 0 || height <= 0 || radius < 0) {
    return NULL;
  }

  uint8_t *result = malloc(width * height);
  if (!result) {
    return NULL;
  }

  // Initialize result to 0 (black)
  memset(result, 0, width * height);

  // For each pixel in the image
  for (int32_t y = 0; y < height; y++) {
    for (int32_t x = 0; x < width; x++) {
      int32_t idx = y * width + x;

      // If current pixel is white (255), dilate it
      if (image_data[idx] == 255) {
        // Apply disk-shaped structuring element
        for (int32_t dy = -radius; dy <= radius; dy++) {
          for (int32_t dx = -radius; dx <= radius; dx++) {
            // Check if point32_t is within disk (use slightly larger radius for
            // better connectivity)
            double r_eff = radius + 0.5;
            if (dx * dx + dy * dy <= r_eff * r_eff) {
              int32_t ny = y + dy;
              int32_t nx = x + dx;

              // Check bounds
              if (ny >= 0 && ny < height && nx >= 0 && nx < width) {
                int32_t nidx = ny * width + nx;
                result[nidx] = 255;
              }
            }
          }
        }
      }
    }
  }

  return result;
}

static uint8_t *binary_erosion_disk(
  uint8_t const *image_data,
  int32_t width,
  int32_t height,
  int32_t radius
) {
  if (!image_data || width <= 0 || height <= 0 || radius < 0) {
    return NULL;
  }

  uint8_t *result = malloc(width * height);
  if (!result) {
    return NULL;
  }

  // Initialize result to 0 (black)
  memset(result, 0, width * height);

  // For each pixel in the image
  for (int32_t y = 0; y < height; y++) {
    for (int32_t x = 0; x < width; x++) {
      int32_t idx = y * width + x;

      // Check if structuring element fits entirely within white pixels
      bool can_erode = true;

      for (int32_t dy = -radius; dy <= radius && can_erode; dy++) {
        for (int32_t dx = -radius; dx <= radius && can_erode; dx++) {
          // Check if point32_t is within disk (use slightly larger radius for
          // better connectivity)
          double r_eff = radius + 0.5;
          if (dx * dx + dy * dy <= r_eff * r_eff) {
            int32_t ny = y + dy;
            int32_t nx = x + dx;

            // Check bounds - pixels outside image are considered black (0)
            if (ny < 0 || ny >= height || nx < 0 || nx >= width) {
              can_erode = false;
            }
            else {
              int32_t nidx = ny * width + nx;
              if (image_data[nidx] != 255) {
                can_erode = false;
              }
            }
          }
        }
      }

      if (can_erode) {
        result[idx] = 255;
      }
    }
  }

  return result;
}

uint8_t *fcv_binary_closing_disk(
  uint8_t const *image_data,
  int32_t width,
  int32_t height,
  int32_t radius
) {
  if (!image_data || width <= 0 || height <= 0 || radius < 0) {
    return NULL;
  }

  // Step 1: Dilation
  uint8_t *dilated = binary_dilation_disk(image_data, width, height, radius);
  if (!dilated) {
    return NULL;
  }

  // Step 2: Erosion of the dilated image
  uint8_t *result = binary_erosion_disk(dilated, width, height, radius);

  // Free intermediate result
  free(dilated);

  return result;
}
// File: src/conversion.c
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

#ifndef FLATCV_AMALGAMATION
#include "conversion.h"
#include "draw.h"
#include "parse_hex_color.h"
#include "perspectivetransform.h"
#include "rgba_to_grayscale.h"
#include "single_to_multichannel.h"
#include "sobel_edge_detection.h"
#else
#include "flatcv.h"
#endif

/**
 * Convert raw RGBA row-major top-to-bottom image data
 * to RGBA row-major top-to-bottom grayscale image data.
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param data Pointer to the pixel data.
 * @return Pointer to the grayscale image data.
 */
uint8_t *
fcv_grayscale(uint32_t width, uint32_t height, uint8_t const *const data) {
  uint32_t img_length_byte = width * height * 4;
  uint8_t *grayscale_data = malloc(img_length_byte);

  if (!grayscale_data) { // Memory allocation failed
    return NULL;
  }

  // Process each pixel row by row
  for (uint32_t i = 0; i < width * height; i++) {
    uint32_t rgba_index = i * 4;

    uint8_t r = data[rgba_index];
    uint8_t g = data[rgba_index + 1];
    uint8_t b = data[rgba_index + 2];

    uint8_t gray = (r * R_WEIGHT + g * G_WEIGHT + b * B_WEIGHT) >> 8;

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
uint8_t *fcv_grayscale_stretch(
  uint32_t width,
  uint32_t height,
  uint8_t const *const data
) {
  uint32_t img_length_byte = width * height * 4;
  uint8_t *grayscale_data = malloc(img_length_byte);

  if (!grayscale_data) { // Memory allocation failed
    return NULL;
  }

  uint32_t img_length_px = width * height;
  // Ignore 1.5625 % of the pixels
  uint32_t num_pixels_to_ignore = img_length_px >> 6;

  uint8_t *gray_values = malloc(img_length_px);
  if (!gray_values) { // Memory allocation failed
    free(grayscale_data);
    return NULL;
  }

  // Process each pixel row by row to get grayscale values
  for (uint32_t i = 0; i < img_length_px; i++) {
    uint32_t rgba_index = i * 4;

    uint8_t r = data[rgba_index];
    uint8_t g = data[rgba_index + 1];
    uint8_t b = data[rgba_index + 2];

    gray_values[i] = (r * R_WEIGHT + g * G_WEIGHT + b * B_WEIGHT) >> 8;
  }

  // Use counting sort to find the 1.5625% darkest and brightest pixels
  uint32_t histogram[256] = {0};
  for (uint32_t i = 0; i < img_length_px; i++) {
    histogram[gray_values[i]]++;
  }

  uint32_t cumulative_count = 0;
  uint8_t min_val = 0;
  for (uint32_t i = 0; i < 256; i++) {
    cumulative_count += histogram[i];
    if (cumulative_count > num_pixels_to_ignore) {
      min_val = i;
      break;
    }
  }

  cumulative_count = 0;
  uint8_t max_val = 255;
  for (int32_t i = 255; i >= 0; i--) {
    cumulative_count += histogram[i];
    if (cumulative_count > num_pixels_to_ignore) {
      max_val = i;
      break;
    }
  }

  free(gray_values);

  uint8_t range = max_val - min_val;

  // Process each pixel row by row
  for (uint32_t i = 0; i < img_length_px; i++) {
    uint32_t rgba_index = i * 4;

    uint8_t r = data[rgba_index];
    uint8_t g = data[rgba_index + 1];
    uint8_t b = data[rgba_index + 2];

    uint8_t gray = (r * R_WEIGHT + g * G_WEIGHT + b * B_WEIGHT) >> 8;

    if (gray < min_val) {
      gray = 0;
    }
    else if (gray > max_val) {
      gray = 255;
    }
    else {
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
 * Apply a global threshold to the image data.
 *
 * @param img_length_px Length of the image data in pixels.
 * @param data Pointer to the image data.
 * @param threshold Threshold value.
 *
 */
void fcv_apply_global_threshold(
  uint32_t img_length_px,
  uint8_t *data,
  uint8_t threshold
) {
  for (uint32_t i = 0; i < img_length_px; i++) {
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
  uint32_t img_length_px,
  uint8_t *data,
  uint8_t lower_threshold,
  uint8_t upper_threshold
) {
  for (uint32_t i = 0; i < img_length_px; i++) {
    if (data[i] < lower_threshold) {
      data[i] = 0;
    }
    else if (data[i] > upper_threshold) {
      data[i] = 255;
    }
    else {
      data[i] =
        (data[i] - lower_threshold) * 255 / (upper_threshold - lower_threshold);
    }
  }
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
uint8_t *fcv_otsu_threshold_rgba(
  uint32_t width,
  uint32_t height,
  bool use_double_threshold,
  uint8_t const *const data
) {
  uint8_t *grayscale_img = fcv_rgba_to_grayscale(width, height, data);
  uint32_t img_length_px = width * height;

  uint32_t histogram[256] = {0};
  for (uint32_t i = 0; i < img_length_px; i++) {
    histogram[grayscale_img[i]]++;
  }

  float histogram_norm[256] = {0};
  for (uint32_t i = 0; i < 256; i++) {
    histogram_norm[i] = (float)histogram[i] / img_length_px;
  }

  float global_mean = 0.0;

  for (uint32_t i = 0; i < 256; i++) {
    global_mean += i * histogram_norm[i];
  }

  float cumulative_sum = 0.0;
  float cumulative_mean = 0.0;
  float max_variance = 0.0;
  int32_t optimal_threshold = 0;

  for (uint32_t i = 0; i < 256; i++) {
    cumulative_sum += histogram_norm[i];
    cumulative_mean += i * histogram_norm[i];

    if (cumulative_sum == 0 || cumulative_sum == 1) {
      continue;
    }

    float mean1 = cumulative_mean / cumulative_sum;
    float mean2 = (global_mean - cumulative_mean) / (1 - cumulative_sum);

    float class_variance =
      cumulative_sum * (1 - cumulative_sum) * (mean1 - mean2) * (mean1 - mean2);

    if (class_variance > max_variance) {
      max_variance = class_variance;
      optimal_threshold = i;
    }
  }

  const int32_t threshold_range_offset = 16;

  if (use_double_threshold) {
    apply_double_threshold(
      img_length_px,
      grayscale_img,
      optimal_threshold - threshold_range_offset,
      optimal_threshold + threshold_range_offset
    );
  }
  else {
    fcv_apply_global_threshold(img_length_px, grayscale_img, optimal_threshold);
  }

  uint8_t *monochrome_data =
    fcv_single_to_multichannel(width, height, grayscale_img);

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
uint8_t *fcv_apply_gaussian_blur(
  uint32_t width,
  uint32_t height,
  double radius,
  uint8_t const *const data
) {
  uint32_t img_length_px = width * height;
  if (radius == 0) {
    return memcpy(malloc(width * height * 4), data, width * height * 4);
  }

  uint8_t *blurred_data = malloc(img_length_px * 4);

  if (!blurred_data) { // Memory allocation failed
    return NULL;
  }

  uint32_t kernel_size = 2 * radius + 1;
  float *kernel = malloc(kernel_size * sizeof(float));

  if (!kernel) { // Memory allocation failed
    free(blurred_data);
    return NULL;
  }

  float sigma = radius / 3.0;
  float sigma_sq = sigma * sigma;
  float two_sigma_sq = 2 * sigma_sq;
  float sqrt_two_pi_sigma = sqrt(2 * M_PI) * sigma;

  for (uint32_t i = 0; i < kernel_size; i++) {
    int32_t x = i - radius;
    kernel[i] = exp(-(x * x) / two_sigma_sq) / sqrt_two_pi_sigma;
  }

  // Apply the kernel in the horizontal direction
  for (uint32_t y = 0; y < height; y++) {
    for (uint32_t x = 0; x < width; x++) {
      float r_sum = 0.0;
      float g_sum = 0.0;
      float b_sum = 0.0;
      float weight_sum = 0.0;

      for (int32_t k = -radius; k <= radius; k++) {
        int32_t x_offset = x + k;
        if (x_offset < 0 || (uint32_t)x_offset >= width) {
          continue;
        }

        uint32_t img_index = y * width + x_offset;
        uint32_t img_rgba_index = img_index * 4;

        // Bounds check for array access
        if (img_rgba_index + 2 >= img_length_px * 4) {
          continue;
        }

        float weight = kernel[k + (int32_t)radius];
        weight_sum += weight;

        r_sum += data[img_rgba_index] * weight;
        g_sum += data[img_rgba_index + 1] * weight;
        b_sum += data[img_rgba_index + 2] * weight;
      }

      uint32_t rgba_index = (y * width + x) * 4;
      blurred_data[rgba_index] = r_sum / weight_sum;
      blurred_data[rgba_index + 1] = g_sum / weight_sum;
      blurred_data[rgba_index + 2] = b_sum / weight_sum;
      blurred_data[rgba_index + 3] = 255;
    }
  }

  // Create temporary buffer for vertical pass to avoid reading from buffer
  // being written to
  uint8_t *temp_data = malloc(img_length_px * 4);
  if (!temp_data) {
    free(blurred_data);
    free(kernel);
    return NULL;
  }

  // Copy horizontal blur result to temp buffer
  memcpy(temp_data, blurred_data, img_length_px * 4);

  // Apply the kernel in the vertical direction
  for (uint32_t x = 0; x < width; x++) {
    for (uint32_t y = 0; y < height; y++) {
      float r_sum = 0.0;
      float g_sum = 0.0;
      float b_sum = 0.0;
      float weight_sum = 0.0;

      for (int32_t k = -radius; k <= radius; k++) {
        int32_t y_offset = y + k;
        if (y_offset < 0 || (uint32_t)y_offset >= height) {
          continue;
        }

        uint32_t img_index = y_offset * width + x;
        uint32_t img_rgba_index = img_index * 4;

        float weight = kernel[k + (int32_t)radius];
        weight_sum += weight;

        r_sum += temp_data[img_rgba_index] * weight;
        g_sum += temp_data[img_rgba_index + 1] * weight;
        b_sum += temp_data[img_rgba_index + 2] * weight;
      }

      uint32_t rgba_index = (y * width + x) * 4;
      blurred_data[rgba_index] = r_sum / weight_sum;
      blurred_data[rgba_index + 1] = g_sum / weight_sum;
      blurred_data[rgba_index + 2] = b_sum / weight_sum;
      blurred_data[rgba_index + 3] = 255;
    }
  }

  free(temp_data);

  free(kernel);

  return blurred_data;
}

#include <time.h>

/**
 * Convert image to anti-aliased black and white.
 * 1. Convert the image to grayscale.
 * 2. Subtract blurred image from the original image to get the high
 * frequencies.
 * 3. Apply OTSU's threshold to get the optimal threshold.
 * 4. Apply the threshold + offset to get the anti-aliased image.
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param use_double_threshold Whether to use double thresholding.
 * @param data Pointer to the pixel data.
 * @return Pointer to the blurred image data.
 */
uint8_t *fcv_bw_smart(
  uint32_t width,
  uint32_t height,
  bool use_double_threshold,
  uint8_t const *const data
) {
  uint8_t *grayscale_data = fcv_grayscale(width, height, data);

  // Calculate blur radius dependent on image size
  // (Empirical formula after testing)
  double blurRadius = (sqrt((double)width * (double)height)) * 0.1;

  uint8_t *blurred_data =
    fcv_apply_gaussian_blur(width, height, blurRadius, grayscale_data);

  uint32_t img_length_px = width * height;
  uint8_t *high_freq_data = malloc(img_length_px * 4);

  if (!high_freq_data) { // Memory allocation failed
    free((void *)grayscale_data);
    free((void *)blurred_data);
    return NULL;
  }

  // Subtract blurred image from the original image to get the high frequencies
  // and invert the high frequencies to get a white background.
  for (uint32_t i = 0; i < img_length_px; i++) {
    uint32_t rgba_idx = i * 4;
    int32_t high_freq_val =
      127 + grayscale_data[rgba_idx] - blurred_data[rgba_idx];

    // Clamp the value to [0, 255] to prevent overflow
    if (high_freq_val < 0) {
      high_freq_val = 0;
    }
    else if (high_freq_val > 255) {
      high_freq_val = 255;
    }

    high_freq_data[rgba_idx] = high_freq_val;     // R
    high_freq_data[rgba_idx + 1] = high_freq_val; // G
    high_freq_data[rgba_idx + 2] = high_freq_val; // B
    high_freq_data[rgba_idx + 3] = 255;           // A
  }

  free((void *)grayscale_data);
  free((void *)blurred_data);

  uint8_t *final_data = fcv_otsu_threshold_rgba(
    width,
    height,
    use_double_threshold,
    high_freq_data
  );

  free(high_freq_data);

  return final_data;
}

/**
 * Resize an image by given resize factors using bilinear interpolation.
 *
 * @param width Width of the input image.
 * @param height Height of the input image.
 * @param resize_x Horizontal resize factor (e.g., 2.0 for 2x, 0.5 for half).
 * @param resize_y Vertical resize factor (e.g., 2.0 for 2x, 0.5 for half).
 * @param out_width Pointer to store the output image width.
 * @param out_height Pointer to store the output image height.
 * @param data Pointer to the input pixel data.
 * @return Pointer to the resized image data.
 */
uint8_t *fcv_resize(
  uint32_t width,
  uint32_t height,
  double resize_x,
  double resize_y,
  uint32_t *out_width,
  uint32_t *out_height,
  uint8_t const *const data
) {
  if (resize_x <= 0.0 || resize_y <= 0.0) {
    return NULL;
  }

  *out_width = (uint32_t)(width * resize_x);
  *out_height = (uint32_t)(height * resize_y);

  if (*out_width == 0 || *out_height == 0) {
    return NULL;
  }

  uint32_t out_img_length = *out_width * *out_height * 4;
  uint8_t *resized_data = malloc(out_img_length);

  if (!resized_data) {
    return NULL;
  }

  for (uint32_t out_y = 0; out_y < *out_height; out_y++) {
    for (uint32_t out_x = 0; out_x < *out_width; out_x++) {
      if (resize_x < 1.0 || resize_y < 1.0) {
        double src_x = (out_x + 0.5) / resize_x - 0.5;
        double src_y = (out_y + 0.5) / resize_y - 0.5;

        double filter_size_x = 1.0 / resize_x;
        double filter_size_y = 1.0 / resize_y;

        double x_start = src_x - filter_size_x * 0.5;
        double y_start = src_y - filter_size_y * 0.5;
        double x_end = src_x + filter_size_x * 0.5;
        double y_end = src_y + filter_size_y * 0.5;

        int32_t ix_start = (int32_t)floor(x_start);
        int32_t iy_start = (int32_t)floor(y_start);
        int32_t ix_end = (int32_t)ceil(x_end);
        int32_t iy_end = (int32_t)ceil(y_end);

        if (ix_start < 0) {
          ix_start = 0;
        }
        if (iy_start < 0) {
          iy_start = 0;
        }
        if (ix_end > (int32_t)width) {
          ix_end = width;
        }
        if (iy_end > (int32_t)height) {
          iy_end = height;
        }

        double r_sum = 0.0, g_sum = 0.0, b_sum = 0.0;
        double total_weight = 0.0;

        for (int32_t sy = iy_start; sy < iy_end; sy++) {
          for (int32_t sx = ix_start; sx < ix_end; sx++) {
            double left = sx;
            double right = sx + 1;
            double top = sy;
            double bottom = sy + 1;

            double overlap_left = left > x_start ? left : x_start;
            double overlap_right = right < x_end ? right : x_end;
            double overlap_top = top > y_start ? top : y_start;
            double overlap_bottom = bottom < y_end ? bottom : y_end;

            if (overlap_right > overlap_left && overlap_bottom > overlap_top) {
              double weight =
                (overlap_right - overlap_left) * (overlap_bottom - overlap_top);
              total_weight += weight;

              uint32_t src_idx = (sy * width + sx) * 4;
              r_sum += data[src_idx] * weight;
              g_sum += data[src_idx + 1] * weight;
              b_sum += data[src_idx + 2] * weight;
            }
          }
        }

        if (total_weight > 0.0) {
          resized_data[(out_y * *out_width + out_x) * 4] =
            (uint8_t)(r_sum / total_weight + 0.5);
          resized_data[(out_y * *out_width + out_x) * 4 + 1] =
            (uint8_t)(g_sum / total_weight + 0.5);
          resized_data[(out_y * *out_width + out_x) * 4 + 2] =
            (uint8_t)(b_sum / total_weight + 0.5);
        }
        else {
          resized_data[(out_y * *out_width + out_x) * 4] = 0;
          resized_data[(out_y * *out_width + out_x) * 4 + 1] = 0;
          resized_data[(out_y * *out_width + out_x) * 4 + 2] = 0;
        }
        resized_data[(out_y * *out_width + out_x) * 4 + 3] = 255;
      }
      else {
        double src_x = (out_x + 0.5) / resize_x - 0.5;
        double src_y = (out_y + 0.5) / resize_y - 0.5;

        int32_t x0 = (int32_t)floor(src_x);
        int32_t y0 = (int32_t)floor(src_y);
        int32_t x1 = x0 + 1;
        int32_t y1 = y0 + 1;

        if (x0 < 0) {
          x0 = 0;
        }
        if (y0 < 0) {
          y0 = 0;
        }
        if (x1 >= (int32_t)width) {
          x1 = width - 1;
        }
        if (y1 >= (int32_t)height) {
          y1 = height - 1;
        }

        double dx = src_x - x0;
        double dy = src_y - y0;

        if (dx < 0) {
          dx = 0;
        }
        if (dy < 0) {
          dy = 0;
        }
        if (dx > 1) {
          dx = 1;
        }
        if (dy > 1) {
          dy = 1;
        }

        for (int32_t c = 0; c < 3; c++) {
          uint8_t p00 = data[(y0 * width + x0) * 4 + c];
          uint8_t p01 = data[(y0 * width + x1) * 4 + c];
          uint8_t p10 = data[(y1 * width + x0) * 4 + c];
          uint8_t p11 = data[(y1 * width + x1) * 4 + c];

          double interpolated = p00 * (1 - dx) * (1 - dy) +
                                p01 * dx * (1 - dy) + p10 * (1 - dx) * dy +
                                p11 * dx * dy;

          resized_data[(out_y * *out_width + out_x) * 4 + c] =
            (uint8_t)(interpolated + 0.5);
        }

        resized_data[(out_y * *out_width + out_x) * 4 + 3] = 255;
      }
    }
  }

  return resized_data;
}
// File: src/convert_to_binary.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef FLATCV_AMALGAMATION
#include "convert_to_binary.h"
#include "parse_hex_color.h"
#else
#include "flatcv.h"
#endif

/**
 * Convert an image to binary format based on foreground and background colors.
 * Return a grayscale image where the foreground color is white and the
 * background color is black.
 *
 * @param image_data Pointer to the input image data (RGBA format).
 * @param width Width of the image.
 * @param height Height of the image.
 * @param foreground_hex Hex color code for the foreground color (e.g.,
 * "FF0000").
 * @param background_hex Hex color code for the background color (e.g.,
 * "00FF00").
 */
uint8_t *fcv_convert_to_binary(
  const uint8_t *image_data,
  int32_t width,
  int32_t height,
  const char *foreground_hex,
  const char *background_hex
) {
  assert(image_data != NULL);
  assert(width > 0);
  assert(height > 0);
  assert(foreground_hex != NULL);
  assert(background_hex != NULL);

  uint8_t *result = malloc(width * height);
  if (!result) {
    fprintf(stderr, "Error: Failed to allocate memory for binary image\n");
    exit(EXIT_FAILURE);
  }

  uint8_t r_fg, g_fg, b_fg;
  fcv_parse_hex_color(foreground_hex, &r_fg, &g_fg, &b_fg);

  uint8_t r_bg, g_bg, b_bg;
  fcv_parse_hex_color(background_hex, &r_bg, &g_bg, &b_bg);

  for (int32_t i = 0; i < width * height * 4; i += 4) {
    uint8_t r = image_data[i];
    uint8_t g = image_data[i + 1];
    uint8_t b = image_data[i + 2];

    int32_t pixel_index = i / 4;
    if (r == r_fg && g == g_fg && b == b_fg) {
      // Convert foreground color to white
      result[pixel_index] = 255;
    }
    else if (r == r_bg && g == g_bg && b == b_bg) {
      // Convert background color to black
      result[pixel_index] = 0;
    }
    else {
      // Default to black for unmatched colors
      result[pixel_index] = 0;
    }
  }

  return result;
}
// File: src/corner_detection.c
#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "binary_closing_disk.h"
#include "conversion.h"
#include "convert_to_binary.h"
#include "corner_peaks.h"
#include "draw.h"
#include "foerstner_corner.h"
#include "parse_hex_color.h"
#include "perspectivetransform.h"
#include "sobel_edge_detection.h"
#include "sort_corners.h"
#include "watershed_segmentation.h"

#ifdef DEBUG_LOGGING
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
#endif
#else
#include "flatcv.h"
#endif

/** Count number of distinct colors in RGBA image
 * 1. Convert to grayscale
 * 2. Scale to 256x256 (save scale ratio for x and y)
 * 3. Blur image
 * 4. Create elevation map with Sobel
 * 5. Flatten elevation map at center seed to avoid being trapped in a local
 * minimum
 * 6. Add black border to flood from all directions at once
 * 7. `watershed(image=elevation_map, markers=markers)`
 *     Set center as marker for foreground basin and border for background basin
 * 8. Check region count equals 2
 * 9. Smooth result with binary closing
 * 10. Use Foerstner corner detector
 *     (Harris detector corners are shifted inwards)
 * 11. Sort corners
 * 12. Select 4 corners with the largest angle while maintaining their order
 * 13. Normalize corners based on scale ratio
 */
int32_t count_colors(const uint8_t *image, int32_t width, int32_t height) {
  assert(image != NULL);
  assert(width > 0);
  assert(height > 0);

  int32_t color_count = 0;
  int32_t *color_map = (int32_t *)calloc(256 * 256 * 256, sizeof(int32_t));
  if (!color_map) {
    fprintf(stderr, "Error: Failed to allocate memory for color map\n");
    return -1;
  }

  for (int32_t i = 0; i < width * height * 4; i += 4) {
    uint8_t r = image[i];
    uint8_t g = image[i + 1];
    uint8_t b = image[i + 2];
    uint8_t a = image[i + 3];

    // Skip fully transparent pixels
    if (a == 0) {
      continue;
    }

    // Create a unique color key
    int32_t color_key = (r << 16) | (g << 8) | b;

    // Increment the count for this color
    if (color_map[color_key] == 0) {
      color_count++;
    }
    color_map[color_key]++;
  }

  free(color_map);
  return color_count;
}

typedef struct {
  int32_t width;
  int32_t height;
  int32_t channels;
  const uint8_t *data;
} Image;

#ifdef DEBUG_LOGGING
void write_debug_img(Image img, const char *filename) {
  int32_t result = stbi_write_png(
    filename,
    img.width,
    img.height,
    img.channels,
    img.data,
    img.width * img.channels
  );
  if (result == 0) {
    fprintf(stderr, "Error: Failed to write debug image %s\n", filename);
  }
}
#endif

/**
 * Detect corners in the input image.
 */
Corners
fcv_detect_corners(const uint8_t *image, int32_t width, int32_t height) {
  assert(image != NULL);
  assert(width > 0);
  assert(height > 0);

  Corners default_corners = {
    .tl_x = 0,
    .tl_y = 0,
    .tr_x = width - 1,
    .tr_y = 0,
    .br_x = width - 1,
    .br_y = height - 1,
    .bl_x = 0,
    .bl_y = height - 1
  };

  // 1. Convert to grayscale
  uint8_t const *grayscale_image = fcv_grayscale(width, height, image);
  if (!grayscale_image) {
    fprintf(stderr, "Error: Failed to convert image to grayscale\n");
    return default_corners;
  }

  // 2. Resize image to 256x256
  uint32_t out_width = 256;
  uint32_t out_height = 256;
  uint8_t const *resized_image = fcv_resize(
    width,
    height,
    (double)out_width / width,
    (double)out_height / height,
    &out_width,
    &out_height,
    grayscale_image
  );
  free((void *)grayscale_image);
  if (!resized_image) {
    fprintf(stderr, "Error: Failed to resize image\n");
    exit(EXIT_FAILURE);
  }

#ifdef DEBUG_LOGGING
  Image out_img = {
    .width = out_width,
    .height = out_height,
    .channels = 4,
    .data = resized_image
  };
  write_debug_img(out_img, "temp_1_resized_image.png");
#endif

  // 3. Apply Gaussian blur
  uint8_t const *blurred_image =
    fcv_apply_gaussian_blur(out_width, out_height, 3.0, resized_image);
#ifndef DEBUG_LOGGING
  free((void *)resized_image);
#endif
  if (!blurred_image) {
    fprintf(stderr, "Error: Failed to apply Gaussian blur\n");
    exit(EXIT_FAILURE);
  }

  // 4. Create elevation map with Sobel edge detection
  uint8_t *elevation_map = (uint8_t *)
    fcv_sobel_edge_detection(out_width, out_height, 4, blurred_image);
  free((void *)blurred_image);
  if (!elevation_map) {
    fprintf(stderr, "Error: Failed to create elevation map with Sobel\n");
    exit(EXIT_FAILURE);
  }

  // 5. Flatten elevation map at center to not get trapped in a local minimum
  fcv_draw_disk(
    out_width,
    out_height,
    1, // Single channel grayscale
    "000000",
    24,
    out_width / 2.0,
    out_height / 2.0,
    elevation_map
  );

  // 6. Add black border to flood from all directions at once
  uint32_t bordered_width, bordered_height;
  uint8_t *bordered_elevation_map = fcv_add_border(
    out_width,
    out_height,
    1,        // Single channel grayscale
    "000000", // Black border
    1,        // Border width of 1 pixel
    elevation_map,
    &bordered_width,
    &bordered_height
  );

  free(elevation_map);
  if (!bordered_elevation_map) {
    fprintf(stderr, "Error: Failed to add black border to elevation map\n");
    exit(EXIT_FAILURE);
  }

#ifdef DEBUG_LOGGING
  out_img.data = bordered_elevation_map;
  out_img.channels = 1; // Grayscale
  out_img.width = bordered_width;
  out_img.height = bordered_height;
  write_debug_img(out_img, "temp_2_elevation_map.png");
#endif

  // 7. Perform watershed segmentation
  int32_t num_markers = 2;
  Point2D *markers = malloc(num_markers * sizeof(Point2D));
  if (!markers) {
    fprintf(stderr, "Error: Failed to allocate memory for markers\n");
    free((void *)bordered_elevation_map);
    exit(EXIT_FAILURE);
  }
  // Set center as foreground marker and upper left corner as background marker
  // Adjust for the added border (border width = 1)
  markers[0] = (Point2D){.x = bordered_width / 2.0, .y = bordered_height / 2.0};
  markers[1] = (Point2D){.x = 0, .y = 0};

  uint8_t *segmented_image_wide = fcv_watershed_segmentation(
    bordered_width,
    bordered_height,
    bordered_elevation_map,
    markers,
    num_markers,
    false // No boundaries
  );
  free((void *)bordered_elevation_map);
  free((void *)markers);

  // Remove 1 pixel border from segmented image
  uint8_t *segmented_image = malloc(out_width * out_height * 4);
  if (!segmented_image) {
    fprintf(stderr, "Error: Failed to allocate memory for segmented image\n");
    free((void *)segmented_image_wide);
    exit(EXIT_FAILURE);
  }
  for (uint32_t y = 1; y < bordered_height - 1; y++) {
    memcpy(
      &segmented_image[(y - 1) * out_width * 4],
      &segmented_image_wide[(y * bordered_width + 1) * 4],
      out_width * 4
    );
  }

#ifdef DEBUG_LOGGING
  out_img.width = out_width;
  out_img.height = out_height;
  out_img.channels = 4;
  out_img.data = segmented_image;
  write_debug_img(out_img, "temp_3_watershed.png");
#endif

  // 8. Check if the image has exactly 2 regions (foreground and background)
  int32_t region_count = count_colors(segmented_image, out_width, out_height);
  if (region_count != 2) {
    fprintf(stderr, "Error: Expected 2 regions, found %d\n", region_count);
    free((void *)segmented_image);
    exit(EXIT_FAILURE);
  }

  // Convert red pixel (marker 1 - foreground) to white
  // and green pixel (marker 2 - background) to black
  uint8_t *segmented_binary = fcv_convert_to_binary(
    segmented_image,
    out_width,
    out_height,
    "FF0000", // red -> white
    "00FF00"  // green -> black
  );
  free((void *)segmented_image);

#ifdef DEBUG_LOGGING
  out_img.data = segmented_binary;
  out_img.channels = 1;
  write_debug_img(out_img, "temp_4_watershed_binary.png");
#endif

  // 9. Smooth the result
  uint8_t *segmented_closed = fcv_binary_closing_disk(
    segmented_binary,
    out_width,
    out_height,
    12 // Closing radius
  );
  free((void *)segmented_binary);
  if (!segmented_closed) {
    fprintf(stderr, "Error: Failed to perform binary closing\n");
    exit(EXIT_FAILURE);
  }

#ifdef DEBUG_LOGGING
  out_img.data = segmented_closed;
  write_debug_img(out_img, "temp_5_segmented_closed.png");
#endif

  // 10. Find corners in the closed image
  uint8_t *corner_response = fcv_foerstner_corner(
    out_width,
    out_height,
    segmented_closed,
    1.5 // Sigma for Gaussian smoothing
  );
  free((void *)segmented_closed);
  if (!corner_response) {
    fprintf(stderr, "Error: Failed to compute corner response\n");
    exit(EXIT_FAILURE);
  }

  // Extract `w` channel (error ellipse size) for visualization
  uint8_t *w_channel = malloc(out_width * out_height);
  if (!w_channel) {
    fprintf(stderr, "Error: Failed to allocate memory for w channel\n");
    free((void *)corner_response);
    exit(EXIT_FAILURE);
  }

  for (uint32_t i = 0; i < out_width * out_height; i++) {
    w_channel[i] = corner_response[i * 2]; // Extract `w` channel
  }

#ifdef DEBUG_LOGGING
  out_img.data = w_channel;
  write_debug_img(out_img, "temp_6_corner_response.png");
#endif
  free(w_channel);

  // 10. Find corner peaks using thresholds
  // Gradually decrease thresholds until it finds at least 4 corners
  CornerPeaks *peaks = NULL;
  double accuracy_thresh = 0.5;
  double roundness_thresh = 0.3;
  const double thresh_decrement = 0.05;
  const double min_thresh = 0.01;

  do {
    if (peaks) {
      free(peaks->points);
      free(peaks);
    }

    peaks = fcv_corner_peaks(
      out_width,
      out_height,
      corner_response,
      16, // Minimum distance between peaks
      accuracy_thresh,
      roundness_thresh
    );

    if (!peaks || peaks->count < 4) {
      accuracy_thresh -= thresh_decrement;
      roundness_thresh -= thresh_decrement;

      // Prevent thresholds from going negative
      if (accuracy_thresh < min_thresh) {
        accuracy_thresh = min_thresh;
      }
      if (roundness_thresh < min_thresh) {
        roundness_thresh = min_thresh;
      }
    }

  } while (peaks && peaks->count < 4 &&
           (accuracy_thresh > min_thresh || roundness_thresh > min_thresh));
  free((void *)corner_response);
  if (!peaks) {
    fprintf(stderr, "Error: Failed to find corner peaks\n");
    exit(EXIT_FAILURE);
  }

  // First, sort all corners to get them in clockwise order
  Point2D *sorted_result = malloc(peaks->count * sizeof(Point2D));
  sort_corners(
    width,
    height,
    out_width,
    out_height,
    peaks->points,
    peaks->count,
    sorted_result
  );

  // Scale corners back to original image dimensions
  double scale_x = (double)width / out_width;
  double scale_y = (double)height / out_height;

  Corners sorted_corners;

  if (peaks->count > 4) {
    // Calculate angles for each corner using cross product method
    double *angles = malloc(peaks->count * sizeof(double));
    typedef struct {
      uint32_t index;
      double angle_abs;
    } AngleIndex;
    AngleIndex *angle_indices = malloc(peaks->count * sizeof(AngleIndex));

    for (uint32_t i = 0; i < peaks->count; i++) {
      // Get the three consecutive points for angle calculation
      Point2D prev = sorted_result[(i - 1 + peaks->count) % peaks->count];
      Point2D curr = sorted_result[i];
      Point2D next = sorted_result[(i + 1) % peaks->count];

      // Calculate vectors from current point
      double a_x = prev.x - curr.x;
      double a_y = prev.y - curr.y;
      double b_x = next.x - curr.x;
      double b_y = next.y - curr.y;

      // Calculate lengths of vectors
      double a_len = sqrt(a_x * a_x + a_y * a_y);
      double b_len = sqrt(b_x * b_x + b_y * b_y);

      if (a_len == 0 || b_len == 0) {
        angles[i] = 0;
      }
      else {
        // Calculate cross product (z-component in 2D)
        double cross_product = (a_x * b_y - a_y * b_x) / (a_len * b_len);
        // Clamp to [-1, 1] to avoid numerical errors
        if (cross_product > 1.0) {
          cross_product = 1.0;
        }
        if (cross_product < -1.0) {
          cross_product = -1.0;
        }

        // Calculate angle in degrees
        angles[i] = asin(cross_product) * 180.0 / M_PI;
      }

      angle_indices[i].index = i;
      angle_indices[i].angle_abs = fabs(angles[i]);
    }

    // Sort indices by descending angle magnitude
    for (uint32_t i = 0; i < peaks->count - 1; i++) {
      for (uint32_t j = 0; j < peaks->count - 1 - i; j++) {
        if (angle_indices[j].angle_abs < angle_indices[j + 1].angle_abs) {
          AngleIndex temp = angle_indices[j];
          angle_indices[j] = angle_indices[j + 1];
          angle_indices[j + 1] = temp;
        }
      }
    }

    // Take top 4 indices and sort them to maintain original clockwise order
    uint32_t top_4_indices[4];
    for (uint32_t i = 0; i < 4; i++) {
      top_4_indices[i] = angle_indices[i].index;
    }

    // Sort the top 4 indices to maintain original clockwise order
    for (uint32_t i = 0; i < 3; i++) {
      for (uint32_t j = 0; j < 3 - i; j++) {
        if (top_4_indices[j] > top_4_indices[j + 1]) {
          uint32_t temp = top_4_indices[j];
          top_4_indices[j] = top_4_indices[j + 1];
          top_4_indices[j + 1] = temp;
        }
      }
    }

    // Create final corners struct directly from selected corners in clockwise
    // order The first sorted corner becomes top-left, and we maintain clockwise
    // sequence
    sorted_corners =
      (Corners){.tl_x = sorted_result[top_4_indices[0]].x * scale_x,
                .tl_y = sorted_result[top_4_indices[0]].y * scale_y,
                .tr_x = sorted_result[top_4_indices[1]].x * scale_x,
                .tr_y = sorted_result[top_4_indices[1]].y * scale_y,
                .br_x = sorted_result[top_4_indices[2]].x * scale_x,
                .br_y = sorted_result[top_4_indices[2]].y * scale_y,
                .bl_x = sorted_result[top_4_indices[3]].x * scale_x,
                .bl_y = sorted_result[top_4_indices[3]].y * scale_y};

    free(angles);
    free(angle_indices);
  }
  else {
    // Use all available corners if we have 4 or fewer, already in clockwise
    // order
    sorted_corners =
      (Corners){.tl_x = sorted_result[0].x * scale_x,
                .tl_y = sorted_result[0].y * scale_y,
                .tr_x = sorted_result[1 % peaks->count].x * scale_x,
                .tr_y = sorted_result[1 % peaks->count].y * scale_y,
                .br_x = sorted_result[2 % peaks->count].x * scale_x,
                .br_y = sorted_result[2 % peaks->count].y * scale_y,
                .bl_x = sorted_result[3 % peaks->count].x * scale_x,
                .bl_y = sorted_result[3 % peaks->count].y * scale_y};
  }

  free(sorted_result);

#ifdef DEBUG_LOGGING
  // Print peaks for debugging
  fprintf(stderr, "Peaks:\n");
  fprintf(stderr, "%.0f,%.0f\n", sorted_corners.tl_x, sorted_corners.tl_y);
  fprintf(stderr, "%.0f,%.0f\n", sorted_corners.tr_x, sorted_corners.tr_y);
  fprintf(stderr, "%.0f,%.0f\n", sorted_corners.br_x, sorted_corners.br_y);
  fprintf(stderr, "%.0f,%.0f\n", sorted_corners.bl_x, sorted_corners.bl_y);
  // Draw corner peaks on the grayscale and resized image
  for (uint32_t i = 0; i < peaks->count; i++) {
    // Print peak coordinates for debugging
    fprintf(
      stderr,
      "Peak %u: (%.1f, %.1f)\n",
      i,
      peaks->points[i].x,
      peaks->points[i].y
    );
    fcv_draw_circle(
      out_width,
      out_height,
      4,        // GRAY as RGBA (each value is the same)
      "FFFF00", // Yellow color for corners
      2,        // Radius
      peaks->points[i].x,
      peaks->points[i].y,
      (uint8_t *)resized_image
    );
  }

  // Draw sorted corners with different colors to distinguish from peaks.
  // Reuse the scale variables from above.
  // (Inverted for drawing on resized image.)
  double draw_scale_x = (double)out_width / width;
  double draw_scale_y = (double)out_height / height;

  // Draw top-left corner in red
  fcv_draw_circle(
    out_width,
    out_height,
    4,
    "FF0000", // Red color for top-left
    3,        // Slightly larger radius to distinguish from peaks
    sorted_corners.tl_x * draw_scale_x,
    sorted_corners.tl_y * draw_scale_y,
    (uint8_t *)resized_image
  );

  // Draw top-right corner in green
  fcv_draw_circle(
    out_width,
    out_height,
    4,
    "00FF00", // Green color for top-right
    3,
    sorted_corners.tr_x * draw_scale_x,
    sorted_corners.tr_y * draw_scale_y,
    (uint8_t *)resized_image
  );

  // Draw bottom-right corner in blue
  fcv_draw_circle(
    out_width,
    out_height,
    4,
    "0000FF", // Blue color for bottom-right
    3,
    sorted_corners.br_x * draw_scale_x,
    sorted_corners.br_y * draw_scale_y,
    (uint8_t *)resized_image
  );

  // Draw bottom-left corner in magenta
  fcv_draw_circle(
    out_width,
    out_height,
    4,
    "FF00FF", // Magenta color for bottom-left
    3,
    sorted_corners.bl_x * draw_scale_x,
    sorted_corners.bl_y * draw_scale_y,
    (uint8_t *)resized_image
  );

  Image corners_img = {
    .width = out_width,
    .height = out_height,
    .channels = 4,
    .data = resized_image
  };
  write_debug_img(corners_img, "temp_7_corners.png");
  free((void *)resized_image);
#endif

  free(peaks);
  return sorted_corners;
}

/**
 * Wrapper function that allocates corners on heap for FFI.
 */
Corners *
fcv_detect_corners_ptr(const uint8_t *image, int32_t width, int32_t height) {
  Corners corners = fcv_detect_corners(image, width, height);
  Corners *result = malloc(sizeof(Corners));
  if (result == NULL) {
    return NULL;
  }
  *result = corners;
  return result;
}
// File: src/corner_peaks.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef FLATCV_AMALGAMATION
#include "1_types.h"
#include "conversion.h"
#include "corner_peaks.h"
#else
#include "flatcv.h"
#endif

static bool is_local_maximum(
  uint8_t const *data,
  uint32_t width,
  uint32_t height,
  uint32_t x,
  uint32_t y,
  uint32_t channel
) {
  if (x == 0 || y == 0 || x >= width - 1 || y >= height - 1) {
    return false;
  }

  uint32_t center_idx = (y * width + x) * 2 + channel;
  uint8_t center_val = data[center_idx];

  if (center_val == 0) {
    return false;
  }

  for (int32_t dy = -1; dy <= 1; dy++) {
    for (int32_t dx = -1; dx <= 1; dx++) {
      if (dx == 0 && dy == 0) {
        continue;
      }

      uint32_t neighbor_idx = ((y + dy) * width + (x + dx)) * 2 + channel;
      if (data[neighbor_idx] > center_val) {
        return false;
      }
    }
  }

  return true;
}

static double euclidean_distance(Point2D a, Point2D b) {
  double dx = a.x - b.x;
  double dy = a.y - b.y;
  return sqrt(dx * dx + dy * dy);
}

/** Detect corner peaks in the image.
 * This function finds local maxima in the corner response image
 * and filters them based on specified thresholds.
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param data Pointer to the 2 channel (w and q) corner response data.
 * @param min_distance Minimum distance between peaks.
 * @param accuracy_thresh Threshold for accuracy measure (w).
 * @param roundness_thresh Threshold for roundness measure (q).
 * @return Pointer to CornerPeaks structure containing detected peaks.
 */
CornerPeaks *fcv_corner_peaks(
  uint32_t width,
  uint32_t height,
  uint8_t const *data,
  uint32_t min_distance,
  double accuracy_thresh,
  double roundness_thresh
) {
  if (!data) {
    return NULL;
  }

  Point2D *candidates = malloc(sizeof(Point2D) * width * height);
  if (!candidates) {
    return NULL;
  }

  uint32_t candidate_count = 0;

  for (uint32_t y = 1; y < height - 1; y++) {
    for (uint32_t x = 1; x < width - 1; x++) {
      uint32_t idx = (y * width + x) * 2;

      // Convert normalized values (0-255) back to 0-1 range
      double w = data[idx] / 255.0;     // accuracy measure
      double q = data[idx + 1] / 255.0; // roundness measure

      if (q > roundness_thresh && w > accuracy_thresh &&
          is_local_maximum(data, width, height, x, y, 0)) {
        candidates[candidate_count].x = (double)x;
        candidates[candidate_count].y = (double)y;
        candidate_count++;
      }
    }
  }

  if (candidate_count == 0) {
    free(candidates);
    CornerPeaks *result = malloc(sizeof(CornerPeaks));
    if (result) {
      result->points = NULL;
      result->count = 0;
    }
    return result;
  }

  bool *rejected = calloc(candidate_count, sizeof(bool));
  if (!rejected) {
    free(candidates);
    return NULL;
  }

  for (uint32_t i = 0; i < candidate_count; i++) {
    if (rejected[i]) {
      continue;
    }

    for (uint32_t j = i + 1; j < candidate_count; j++) {
      if (rejected[j]) {
        continue;
      }

      if (euclidean_distance(candidates[i], candidates[j]) < min_distance) {
        uint32_t i_idx =
          ((uint32_t)candidates[i].y * width + (uint32_t)candidates[i].x) * 2;
        uint32_t j_idx =
          ((uint32_t)candidates[j].y * width + (uint32_t)candidates[j].x) * 2;

        if (data[i_idx] >= data[j_idx]) {
          rejected[j] = true;
        }
        else {
          rejected[i] = true;
          break;
        }
      }
    }
  }

  uint32_t final_count = 0;
  for (uint32_t i = 0; i < candidate_count; i++) {
    if (!rejected[i]) {
      final_count++;
    }
  }

  Point2D *final_points = malloc(sizeof(Point2D) * final_count);
  if (!final_points) {
    free(candidates);
    free(rejected);
    return NULL;
  }

  uint32_t idx = 0;
  for (uint32_t i = 0; i < candidate_count; i++) {
    if (!rejected[i]) {
      final_points[idx] = candidates[i];
      idx++;
    }
  }

  free(candidates);
  free(rejected);

  CornerPeaks *result = malloc(sizeof(CornerPeaks));
  if (!result) {
    free(final_points);
    return NULL;
  }

  result->points = final_points;
  result->count = final_count;

  return result;
}
// File: src/crop.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "crop.h"
#else
#include "flatcv.h"
#endif

/**
 * Crop an image.
 *
 * @param width Width of the original image.
 * @param height Height of the original image.
 * @param channels Number of channels in the image.
 * @param data Pointer to the pixel data.
 * @param x The x-coordinate of the top-left corner of the crop area.
 * @param y The y-coordinate of the top-left corner of the crop area.
 * @param new_width The width of the crop area.
 * @param new_height The height of the crop area.
 * @return Pointer to the new cropped image data.
 */
uint8_t *fcv_crop(
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  uint8_t const *const data,
  uint32_t x,
  uint32_t y,
  uint32_t new_width,
  uint32_t new_height
) {
  if (x + new_width > width || y + new_height > height) {
    fprintf(stderr, "Crop area is outside the original image bounds.\n");
    return NULL;
  }

  uint8_t *cropped_data = malloc(new_width * new_height * channels);
  if (!cropped_data) {
    fprintf(stderr, "Memory allocation failed for cropped image.\n");
    return NULL;
  }

  uint32_t row_bytes = new_width * channels;
  for (uint32_t i = 0; i < new_height; ++i) {
    uint32_t src_index = ((y + i) * width + x) * channels;
    uint32_t dst_index = i * row_bytes;
    memcpy(cropped_data + dst_index, data + src_index, row_bytes);
  }

  return cropped_data;
}
// File: src/draw.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "draw.h"
#include "parse_hex_color.h"
#else
#include "flatcv.h"
#endif

/**
 * Helper function to set a pixel to specified color (for circle drawing)
 */
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
) {
  if (px >= 0 && px < (int32_t)width && py >= 0 && py < (int32_t)height) {
    uint32_t pixel_index = (py * width + px) * channels;

    if (channels == 1) {
      // Grayscale: use luminance formula
      data[pixel_index] = (uint8_t)(0.299 * r + 0.587 * g + 0.114 * b);
    }
    else if (channels >= 3) {
      data[pixel_index] = r;     // R
      data[pixel_index + 1] = g; // G
      data[pixel_index + 2] = b; // B
      if (channels == 4) {
        data[pixel_index + 3] = 255; // A
      }
    }
  }
}

/**
 * Helper function to draw 8 symmetric points of a circle
 */
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
) {
  fcv_set_circle_pixel(data, width, height, channels, cx + x, cy + y, r, g, b);
  fcv_set_circle_pixel(data, width, height, channels, cx - x, cy + y, r, g, b);
  fcv_set_circle_pixel(data, width, height, channels, cx + x, cy - y, r, g, b);
  fcv_set_circle_pixel(data, width, height, channels, cx - x, cy - y, r, g, b);
  fcv_set_circle_pixel(data, width, height, channels, cx + y, cy + x, r, g, b);
  fcv_set_circle_pixel(data, width, height, channels, cx - y, cy + x, r, g, b);
  fcv_set_circle_pixel(data, width, height, channels, cx + y, cy - x, r, g, b);
  fcv_set_circle_pixel(data, width, height, channels, cx - y, cy - x, r, g, b);
}

/**
 * Helper function to fill horizontal lines for a filled circle (disk)
 */
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
) {
  // Fill horizontal lines at y offsets
  for (int32_t i = cx - x; i <= cx + x; i++) {
    fcv_set_circle_pixel(data, width, height, channels, i, cy + y, r, g, b);
    fcv_set_circle_pixel(data, width, height, channels, i, cy - y, r, g, b);
  }

  // Fill horizontal lines at x offsets
  // (avoid duplicating center line when x == y)
  if (x != y) {
    for (int32_t i = cx - y; i <= cx + y; i++) {
      fcv_set_circle_pixel(data, width, height, channels, i, cy + x, r, g, b);
      fcv_set_circle_pixel(data, width, height, channels, i, cy - x, r, g, b);
    }
  }
}

/**
 * Draw a circle on an image using Bresenham's circle algorithm
 * (modifies in-place).
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param channels Number of channels in the image (1, 3, or 4).
 * @param hex_color Hex color code (e.g., "FF0000" for red, "#00FF00" for
 * green).
 * @param radius Radius of the circle.
 * @param center_x X coordinate of the circle center.
 * @param center_y Y coordinate of the circle center.
 * @param data Pointer to the pixel data (modified in-place).
 */
void fcv_draw_circle(
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  const char *hex_color,
  double radius,
  double center_x,
  double center_y,
  uint8_t *data
) {
  if (!data) {
    return;
  }

  // Parse color
  uint8_t r, g, b;
  fcv_parse_hex_color(hex_color, &r, &g, &b);

  int32_t radius_int32_t = (int32_t)radius;
  int32_t cx = (int32_t)center_x;
  int32_t cy = (int32_t)center_y;

  // Bresenham's circle algorithm
  int32_t x = 0;
  int32_t y = radius_int32_t;
  int32_t d = 3 - 2 * radius_int32_t;

  // Initial points
  fcv_draw_circle_points(data, width, height, channels, cx, cy, x, y, r, g, b);

  while (y >= x) {
    x++;
    if (d > 0) {
      y--;
      d = d + 4 * (x - y) + 10;
    }
    else {
      d = d + 4 * x + 6;
    }
    fcv_draw_circle_points(
      data,
      width,
      height,
      channels,
      cx,
      cy,
      x,
      y,
      r,
      g,
      b
    );
  }
}

/**
 * Draw a filled circle (disk) on an image using Bresenham's circle
 * algorithm (modifies in-place).
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param channels Number of channels in the image (1, 3, or 4).
 * @param hex_color Hex color code (e.g., "FF0000" for red, "#00FF00" for
 * green).
 * @param radius Radius of the disk.
 * @param center_x X coordinate of the disk center.
 * @param center_y Y coordinate of the disk center.
 * @param data Pointer to the pixel data (modified in-place).
 */
void fcv_draw_disk(
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  const char *hex_color,
  double radius,
  double center_x,
  double center_y,
  uint8_t *data
) {
  if (!data) {
    return;
  }

  // Parse color
  uint8_t r, g, b;
  fcv_parse_hex_color(hex_color, &r, &g, &b);

  int32_t radius_int32_t = (int32_t)radius;
  int32_t cx = (int32_t)center_x;
  int32_t cy = (int32_t)center_y;

  // Bresenham's circle algorithm for filled disk
  int32_t x = 0;
  int32_t y = radius_int32_t;
  int32_t d = 3 - 2 * radius_int32_t;

  // Initial filled lines
  fcv_fill_disk_lines(data, width, height, channels, cx, cy, x, y, r, g, b);

  while (y >= x) {
    x++;
    if (d > 0) {
      y--;
      d = d + 4 * (x - y) + 10;
    }
    else {
      d = d + 4 * x + 6;
    }
    fcv_fill_disk_lines(data, width, height, channels, cx, cy, x, y, r, g, b);
  }
}

/**
 * Add a colored border around an image.
 * Returns a new image with the border added.
 *
 * @param width Width of the input image.
 * @param height Height of the input image.
 * @param channels Number of channels in the image (1, 3, or 4).
 * @param hex_color Hex color code for the border (e.g., "FF0000" for red).
 * @param border_width Width of the border in pixels.
 * @param input_data Pointer to the input pixel data.
 * @param output_width Pointer to store the output image width.
 * @param output_height Pointer to store the output image height.
 * @return Pointer to the new image data with border, or NULL on failure.
 */
uint8_t *fcv_add_border(
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  const char *hex_color,
  uint32_t border_width,
  uint8_t *input_data,
  uint32_t *output_width,
  uint32_t *output_height
) {
  if (!input_data || !output_width || !output_height || border_width == 0) {
    return NULL;
  }

  // Parse border color
  uint8_t r, g, b;
  fcv_parse_hex_color(hex_color, &r, &g, &b);

  // Calculate output dimensions
  *output_width = width + 2 * border_width;
  *output_height = height + 2 * border_width;

  // Allocate memory for output image
  size_t output_size = *output_width * *output_height * channels;
  uint8_t *output_data = malloc(output_size);
  if (!output_data) {
    return NULL;
  }

  // Fill the entire output image with border color
  for (uint32_t y = 0; y < *output_height; y++) {
    for (uint32_t x = 0; x < *output_width; x++) {
      uint32_t pixel_index = (y * *output_width + x) * channels;

      if (channels == 1) {
        // Grayscale: use luminance formula
        output_data[pixel_index] = (uint8_t)(0.299 * r + 0.587 * g + 0.114 * b);
      }
      else if (channels >= 3) {
        output_data[pixel_index] = r;     // R
        output_data[pixel_index + 1] = g; // G
        output_data[pixel_index + 2] = b; // B
        if (channels == 4) {
          output_data[pixel_index + 3] = 255; // A
        }
      }
    }
  }

  // Copy the original image into the center
  for (uint32_t y = 0; y < height; y++) {
    for (uint32_t x = 0; x < width; x++) {
      uint32_t src_index = (y * width + x) * channels;
      uint32_t dst_index =
        ((y + border_width) * *output_width + (x + border_width)) * channels;

      for (uint32_t c = 0; c < channels; c++) {
        output_data[dst_index + c] = input_data[src_index + c];
      }
    }
  }

  return output_data;
}
// File: src/extract_document.c
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef FLATCV_AMALGAMATION
#include "perspectivetransform.h"
#else
#include "flatcv.h"
#endif

uint8_t *fcv_extract_document(
  uint32_t width,
  uint32_t height,
  uint8_t const *const data,
  uint32_t output_width,
  uint32_t output_height
) {
  if (!data) {
    fprintf(stderr, "Error: NULL image data\n");
    return NULL;
  }

  // Step 1: Detect corners of the document
  Corners detected_corners = fcv_detect_corners(data, width, height);

  // Step 2: Define destination corners (rectangular output)
  Corners dst_corners = {
    .tl_x = 0.0,
    .tl_y = 0.0,
    .tr_x = (double)(output_width - 1),
    .tr_y = 0.0,
    .br_x = (double)(output_width - 1),
    .br_y = (double)(output_height - 1),
    .bl_x = 0.0,
    .bl_y = (double)(output_height - 1)
  };

  // Step 3: Calculate perspective transformation matrix
  // Note: We need to map FROM destination TO source for the inverse transform
  Matrix3x3 *transform_matrix =
    fcv_calculate_perspective_transform(&dst_corners, &detected_corners);

  if (!transform_matrix) {
    fprintf(stderr, "Error: Failed to calculate perspective transform\n");
    return NULL;
  }

  // Step 4: Apply the transformation
  uint8_t *result = fcv_apply_matrix_3x3(
    width,
    height,
    (uint8_t *)data,
    output_width,
    output_height,
    transform_matrix
  );

  // Free the transformation matrix if it's not the identity matrix
  if (transform_matrix &&
      !(transform_matrix->m00 == 1.0 && transform_matrix->m01 == 0.0 &&
        transform_matrix->m02 == 0.0 && transform_matrix->m10 == 0.0 &&
        transform_matrix->m11 == 1.0 && transform_matrix->m12 == 0.0 &&
        transform_matrix->m20 == 0.0 && transform_matrix->m21 == 0.0 &&
        transform_matrix->m22 == 1.0)) {
    free(transform_matrix);
  }

  return result;
}

uint8_t *fcv_extract_document_auto(
  uint32_t width,
  uint32_t height,
  uint8_t const *const data,
  uint32_t *output_width,
  uint32_t *output_height
) {
  if (!data || !output_width || !output_height) {
    fprintf(stderr, "Error: NULL parameters\n");
    return NULL;
  }

  // Step 1: Detect corners of the document
  Corners detected_corners = fcv_detect_corners(data, width, height);

  // Step 2: Calculate optimal output dimensions based on detected corners
  // Calculate distances between corners to determine aspect ratio
  double top_width = sqrt(
    pow(detected_corners.tr_x - detected_corners.tl_x, 2) +
    pow(detected_corners.tr_y - detected_corners.tl_y, 2)
  );
  double bottom_width = sqrt(
    pow(detected_corners.br_x - detected_corners.bl_x, 2) +
    pow(detected_corners.br_y - detected_corners.bl_y, 2)
  );
  double left_height = sqrt(
    pow(detected_corners.bl_x - detected_corners.tl_x, 2) +
    pow(detected_corners.bl_y - detected_corners.tl_y, 2)
  );
  double right_height = sqrt(
    pow(detected_corners.br_x - detected_corners.tr_x, 2) +
    pow(detected_corners.br_y - detected_corners.tr_y, 2)
  );

  // Use maximum dimensions to preserve detail
  double max_width = fmax(top_width, bottom_width);
  double max_height = fmax(left_height, right_height);

  // Round to reasonable dimensions
  *output_width = (uint32_t)(max_width + 0.5);
  *output_height = (uint32_t)(max_height + 0.5);

  // Ensure minimum reasonable size
  if (*output_width < 100) {
    *output_width = 100;
  }
  if (*output_height < 100) {
    *output_height = 100;
  }

  // Step 3: Define destination corners (rectangular output)
  Corners dst_corners = {
    .tl_x = 0.0,
    .tl_y = 0.0,
    .tr_x = (double)(*output_width - 1),
    .tr_y = 0.0,
    .br_x = (double)(*output_width - 1),
    .br_y = (double)(*output_height - 1),
    .bl_x = 0.0,
    .bl_y = (double)(*output_height - 1)
  };

  // Step 4: Calculate perspective transformation matrix
  // Note: We need to map FROM destination TO source for the inverse transform
  Matrix3x3 *transform_matrix =
    fcv_calculate_perspective_transform(&dst_corners, &detected_corners);

  if (!transform_matrix) {
    fprintf(stderr, "Error: Failed to calculate perspective transform\n");
    return NULL;
  }

  // Step 5: Apply the transformation
  uint8_t *result = fcv_apply_matrix_3x3(
    width,
    height,
    (uint8_t *)data,
    *output_width,
    *output_height,
    transform_matrix
  );

  // Free the transformation matrix if it's not the identity matrix
  if (transform_matrix &&
      !(transform_matrix->m00 == 1.0 && transform_matrix->m01 == 0.0 &&
        transform_matrix->m02 == 0.0 && transform_matrix->m10 == 0.0 &&
        transform_matrix->m11 == 1.0 && transform_matrix->m12 == 0.0 &&
        transform_matrix->m20 == 0.0 && transform_matrix->m21 == 0.0 &&
        transform_matrix->m22 == 1.0)) {
    free(transform_matrix);
  }

  return result;
}
// File: src/flip.c
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifndef FLATCV_AMALGAMATION
#include "flip.h"
#else
#include "flatcv.h"
#endif

/**
 * Flip an image horizontally (mirror along vertical axis).
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param data Pointer to the pixel data.
 * @return Pointer to the flipped image data.
 */
uint8_t *
fcv_flip_x(uint32_t width, uint32_t height, uint8_t const *const data) {
  uint32_t img_length_byte = width * height * 4;
  uint8_t *flipped_data = malloc(img_length_byte);

  if (!flipped_data) { // Memory allocation failed
    return NULL;
  }

  for (uint32_t y = 0; y < height; y++) {
    for (uint32_t x = 0; x < width; x++) {
      uint32_t src_index = (y * width + x) * 4;
      uint32_t dst_index = (y * width + (width - 1 - x)) * 4;

      flipped_data[dst_index] = data[src_index];         // R
      flipped_data[dst_index + 1] = data[src_index + 1]; // G
      flipped_data[dst_index + 2] = data[src_index + 2]; // B
      flipped_data[dst_index + 3] = data[src_index + 3]; // A
    }
  }

  return flipped_data;
}

/**
 * Flip an image vertically (mirror along horizontal axis).
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param data Pointer to the pixel data.
 * @return Pointer to the flipped image data.
 */
uint8_t *
fcv_flip_y(uint32_t width, uint32_t height, uint8_t const *const data) {
  uint32_t img_length_byte = width * height * 4;
  uint8_t *flipped_data = malloc(img_length_byte);

  if (!flipped_data) { // Memory allocation failed
    return NULL;
  }

  for (uint32_t y = 0; y < height; y++) {
    for (uint32_t x = 0; x < width; x++) {
      uint32_t src_index = (y * width + x) * 4;
      uint32_t dst_index = ((height - 1 - y) * width + x) * 4;

      flipped_data[dst_index] = data[src_index];         // R
      flipped_data[dst_index + 1] = data[src_index + 1]; // G
      flipped_data[dst_index + 2] = data[src_index + 2]; // B
      flipped_data[dst_index + 3] = data[src_index + 3]; // A
    }
  }

  return flipped_data;
}
// File: src/foerstner_corner.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "conversion.h"
#include "perspectivetransform.h"
#else
#include "flatcv.h"
#endif

/** Implementation of the Foerstner corner measure response image.
 * Expected input is a grayscale image.
 */
uint8_t *fcv_foerstner_corner(
  uint32_t width,
  uint32_t height,
  uint8_t const *const gray_data,
  double sigma
) {
  if (!gray_data || width == 0 || height == 0) {
    return NULL;
  }

  // Allocate memory for gradient images
  double *grad_x = calloc(width * height, sizeof(double));
  double *grad_y = calloc(width * height, sizeof(double));

  if (!grad_x || !grad_y) {
    free(grad_x);
    free(grad_y);
    return NULL;
  }

  // Compute image gradients using Sobel-like operators
  for (uint32_t y = 1; y < height - 1; y++) {
    for (uint32_t x = 1; x < width - 1; x++) {
      uint32_t idx = y * width + x;

      // Sobel X kernel: [-1 0 1; -2 0 2; -1 0 1]
      double gx = 0.0;
      gx += -1.0 * gray_data[(y - 1) * width + (x - 1)];
      gx += 1.0 * gray_data[(y - 1) * width + (x + 1)];
      gx += -2.0 * gray_data[y * width + (x - 1)];
      gx += 2.0 * gray_data[y * width + (x + 1)];
      gx += -1.0 * gray_data[(y + 1) * width + (x - 1)];
      gx += 1.0 * gray_data[(y + 1) * width + (x + 1)];

      // Sobel Y kernel: [-1 -2 -1; 0 0 0; 1 2 1]
      double gy = 0.0;
      gy += -1.0 * gray_data[(y - 1) * width + (x - 1)];
      gy += -2.0 * gray_data[(y - 1) * width + x];
      gy += -1.0 * gray_data[(y - 1) * width + (x + 1)];
      gy += 1.0 * gray_data[(y + 1) * width + (x - 1)];
      gy += 2.0 * gray_data[(y + 1) * width + x];
      gy += 1.0 * gray_data[(y + 1) * width + (x + 1)];

      grad_x[idx] = gx / 8.0; // Normalize
      grad_y[idx] = gy / 8.0;
    }
  }

  // Compute gradient products for structure tensor
  double *Axx = malloc(width * height * sizeof(double));
  double *Axy = malloc(width * height * sizeof(double));
  double *Ayy = malloc(width * height * sizeof(double));

  if (!Axx || !Axy || !Ayy) {
    free(grad_x);
    free(grad_y);
    free(Axx);
    free(Axy);
    free(Ayy);
    return NULL;
  }

  for (uint32_t i = 0; i < width * height; i++) {
    Axx[i] = grad_x[i] * grad_x[i];
    Axy[i] = grad_x[i] * grad_y[i];
    Ayy[i] = grad_y[i] * grad_y[i];
  }

  // Apply Gaussian smoothing to structure tensor elements
  // For simplicity, use a basic box filter approximation when sigma is small
  int32_t kernel_size = (int32_t)(3 * sigma) | 1; // Ensure odd size
  if (kernel_size < 3) {
    kernel_size = 3;
  }
  int32_t half_kernel = kernel_size / 2;

  double *Axx_smooth = calloc(width * height, sizeof(double));
  double *Axy_smooth = calloc(width * height, sizeof(double));
  double *Ayy_smooth = calloc(width * height, sizeof(double));

  if (!Axx_smooth || !Axy_smooth || !Ayy_smooth) {
    free(grad_x);
    free(grad_y);
    free(Axx);
    free(Axy);
    free(Ayy);
    free(Axx_smooth);
    free(Axy_smooth);
    free(Ayy_smooth);
    return NULL;
  }

  // Simple box filter smoothing
  for (uint32_t y = half_kernel; y < height - half_kernel; y++) {
    for (uint32_t x = half_kernel; x < width - half_kernel; x++) {
      uint32_t idx = y * width + x;
      double sum_xx = 0.0, sum_xy = 0.0, sum_yy = 0.0;
      int32_t count = 0;

      for (int32_t ky = -half_kernel; ky <= half_kernel; ky++) {
        for (int32_t kx = -half_kernel; kx <= half_kernel; kx++) {
          int32_t sample_idx = (y + ky) * width + (x + kx);
          sum_xx += Axx[sample_idx];
          sum_xy += Axy[sample_idx];
          sum_yy += Ayy[sample_idx];
          count++;
        }
      }

      Axx_smooth[idx] = sum_xx / count;
      Axy_smooth[idx] = sum_xy / count;
      Ayy_smooth[idx] = sum_yy / count;
    }
  }

  // Compute Foerstner measures w and q
  uint8_t *result = malloc(width * height * 2); // 2 channels: w, q
  if (!result) {
    free(grad_x);
    free(grad_y);
    free(Axx);
    free(Axy);
    free(Ayy);
    free(Axx_smooth);
    free(Axy_smooth);
    free(Ayy_smooth);
    return NULL;
  }

  double max_w = 0.0, max_q = 0.0;
  double *w_values = malloc(width * height * sizeof(double));
  double *q_values = malloc(width * height * sizeof(double));
  if (!w_values || !q_values) {
    free(result);
    free(w_values);
    free(q_values);
    free(grad_x);
    free(grad_y);
    free(Axx);
    free(Axy);
    free(Ayy);
    free(Axx_smooth);
    free(Axy_smooth);
    free(Ayy_smooth);
    return NULL;
  }

  // First pass: compute w and q values and find maximum for normalization
  for (uint32_t i = 0; i < width * height; i++) {
    double det_A =
      Axx_smooth[i] * Ayy_smooth[i] - Axy_smooth[i] * Axy_smooth[i];
    double trace_A = Axx_smooth[i] + Ayy_smooth[i];

    double w = 0.0, q = 0.0;

    if (fabs(trace_A) > 1e-10) { // Avoid division by zero
      w = det_A / trace_A;
      q = 4.0 * det_A / (trace_A * trace_A);
    }

    w_values[i] = fmax(0.0, w); // Ensure non-negative
    q_values[i] = fmax(0.0, q);

    if (w_values[i] > max_w) {
      max_w = w_values[i];
    }
    if (q_values[i] > max_q) {
      max_q = q_values[i];
    }
  }

  // Second pass: normalize and convert to bytes
  for (uint32_t i = 0; i < width * height; i++) {
    uint8_t w_byte = 0, q_byte = 0;

    if (max_w > 0.0) {
      w_byte = (uint8_t)(255.0 * w_values[i] / max_w);
    }
    if (max_q > 0.0) {
      q_byte = (uint8_t)(255.0 * q_values[i] / max_q);
    }

    result[i * 2] = w_byte;     // w measure
    result[i * 2 + 1] = q_byte; // q measure
  }

  free(w_values);
  free(q_values);

  // Cleanup
  free(grad_x);
  free(grad_y);
  free(Axx);
  free(Axy);
  free(Ayy);
  free(Axx_smooth);
  free(Axy_smooth);
  free(Ayy_smooth);

  return result;
}
// File: src/histogram.c
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef FLATCV_AMALGAMATION
#include "histogram.h"
#else
#include "flatcv.h"
#endif

/**
 * Generate a histogram visualization image from input image data.
 * For grayscale images, creates a single histogram.
 * For RGB(A) images, creates overlapping histograms for each channel.
 *
 * @param width Width of the input image.
 * @param height Height of the input image.
 * @param channels Number of channels in the input image (1, 3, or 4).
 * @param data Pointer to the input pixel data.
 * @param out_width Pointer to store the output histogram width.
 * @param out_height Pointer to store the output histogram height.
 * @return Pointer to the histogram image data (RGBA format).
 */
uint8_t *fcv_generate_histogram(
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  uint8_t const *const data,
  uint32_t *out_width,
  uint32_t *out_height
) {
  if (!data || !out_width || !out_height) {
    return NULL;
  }

  // Output histogram dimensions
  const uint32_t hist_width = 256;  // One pixel per brightness value
  const uint32_t hist_height = 200; // Fixed height for visualization

  *out_width = hist_width;
  *out_height = hist_height;

  uint32_t img_size = width * height;
  uint32_t output_size = (*out_width) * (*out_height) * 4;

  // Allocate output image (initialized to black background)
  uint8_t *output = calloc(output_size, sizeof(uint8_t));
  if (!output) {
    return NULL;
  }

  // Initialize with black background and full alpha
  for (uint32_t i = 0; i < (*out_width) * (*out_height); i++) {
    output[i * 4 + 3] = 255; // Alpha channel
  }

  // Count histograms for each channel
  uint32_t hist_r[256] = {0};
  uint32_t hist_g[256] = {0};
  uint32_t hist_b[256] = {0};
  uint32_t hist_gray[256] = {0};

  bool is_grayscale = (channels == 1);
  if (channels == 4) {
    // Check if it's actually grayscale (all RGB channels equal)
    is_grayscale = true;
    for (uint32_t i = 0; i < img_size && is_grayscale; i++) {
      uint32_t idx = i * 4;
      if (data[idx] != data[idx + 1] || data[idx] != data[idx + 2]) {
        is_grayscale = false;
      }
    }
  }

  if (is_grayscale) {
    // Single channel or grayscale image
    for (uint32_t i = 0; i < img_size; i++) {
      uint8_t value;
      if (channels == 1) {
        value = data[i];
      }
      else {
        // Use the red channel for grayscale RGBA
        value = data[i * 4];
      }
      hist_gray[value]++;
    }
  }
  else {
    // RGB or RGBA image - count each channel
    for (uint32_t i = 0; i < img_size; i++) {
      uint32_t idx = i * channels;
      hist_r[data[idx]]++;
      if (channels >= 3) {
        hist_g[data[idx + 1]]++;
        hist_b[data[idx + 2]]++;
      }
    }
  }

  // Find maximum count for scaling
  uint32_t max_count = 0;
  if (is_grayscale) {
    for (int i = 0; i < 256; i++) {
      if (hist_gray[i] > max_count) {
        max_count = hist_gray[i];
      }
    }
  }
  else {
    for (int i = 0; i < 256; i++) {
      if (hist_r[i] > max_count) {
        max_count = hist_r[i];
      }
      if (channels >= 3) {
        if (hist_g[i] > max_count) {
          max_count = hist_g[i];
        }
        if (hist_b[i] > max_count) {
          max_count = hist_b[i];
        }
      }
    }
  }

  if (max_count == 0) {
    return output; // Return black image if no data
  }

  // Draw histogram bars
  for (int x = 0; x < 256; x++) {
    if (is_grayscale) {
      // Draw white histogram for grayscale
      uint32_t bar_height = (hist_gray[x] * hist_height) / max_count;
      for (uint32_t y = 0; y < bar_height; y++) {
        uint32_t output_x = x;
        uint32_t output_y = hist_height - 1 - y;
        uint32_t output_idx = (output_y * (*out_width) + output_x) * 4;

        output[output_idx] = 255;     // R
        output[output_idx + 1] = 255; // G
        output[output_idx + 2] = 255; // B
        output[output_idx + 3] = 255; // A
      }
    }
    else {
      // Draw overlapping colored histograms for RGB
      uint32_t bar_height_r = (hist_r[x] * hist_height) / max_count;
      uint32_t bar_height_g =
        (channels >= 3) ? (hist_g[x] * hist_height) / max_count : 0;
      uint32_t bar_height_b =
        (channels >= 3) ? (hist_b[x] * hist_height) / max_count : 0;

      uint32_t max_bar_height = bar_height_r;
      if (bar_height_g > max_bar_height) {
        max_bar_height = bar_height_g;
      }
      if (bar_height_b > max_bar_height) {
        max_bar_height = bar_height_b;
      }

      for (uint32_t y = 0; y < max_bar_height; y++) {
        uint32_t output_x = x;
        uint32_t output_y = hist_height - 1 - y;
        uint32_t output_idx = (output_y * (*out_width) + output_x) * 4;

        // Initialize to black
        uint8_t r = 0, g = 0, b = 0;

        // Add red channel contribution
        if (y < bar_height_r) {
          r = 255;
        }

        // Add green channel contribution
        if (channels >= 3 && y < bar_height_g) {
          g = 255;
        }

        // Add blue channel contribution
        if (channels >= 3 && y < bar_height_b) {
          b = 255;
        }

        output[output_idx] = r;       // R
        output[output_idx + 1] = g;   // G
        output[output_idx + 2] = b;   // B
        output[output_idx + 3] = 255; // A
      }
    }
  }

  return output;
}
// File: src/parse_hex_color.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "parse_hex_color.h"
#else
#include "flatcv.h"
#endif

// Helper function to parse hex color code to RGB values
void fcv_parse_hex_color(
  const char *hex_color,
  uint8_t *r,
  uint8_t *g,
  uint8_t *b
) {
  const char *hex = hex_color;

  // Skip '#' if present
  if (hex[0] == '#') {
    hex++;
  }

  // Validate hex string length (should be 6 characters)
  if (strlen(hex) != 6) {
    // Default to white for invalid hex codes
    *r = 255;
    *g = 255;
    *b = 255;
    return;
  }

  // Validate that all characters are valid hex digits
  for (int32_t i = 0; i < 6; i++) {
    if (!((hex[i] >= '0' && hex[i] <= '9') ||
          (hex[i] >= 'A' && hex[i] <= 'F') ||
          (hex[i] >= 'a' && hex[i] <= 'f'))) {
      // Default to white for invalid hex codes
      *r = 255;
      *g = 255;
      *b = 255;
      return;
    }
  }

  // Parse hex values
  uint32_t rgb_value;
  sscanf(hex, "%x", &rgb_value);

  *r = (rgb_value >> 16) & 0xFF;
  *g = (rgb_value >> 8) & 0xFF;
  *b = rgb_value & 0xFF;
}
// File: src/perspectivetransform.c
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
int32_t solve_linear_system(double A[8][8], double b[8], double x[8]) {
  const int32_t n = 8;
  const double epsilon = 1e-10;
  int32_t i, j, k;

  // Create augmented matrix [A|b] with extra safety margin
  double aug[8][10]; // One extra column for safety

  // Initialize augmented matrix
  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++) {
      aug[i][j] = A[i][j];
    }
    aug[i][n] = b[i];
  }

  // Gaussian elimination with partial pivoting
  for (i = 0; i < n; i++) {
    // Find pivot
    int32_t max_row = i;
    double max_val = fabs(aug[i][i]);

    for (k = i + 1; k < n; k++) {
      if (fabs(aug[k][i]) > max_val) {
        max_val = fabs(aug[k][i]);
        max_row = k;
      }
    }

    // Check for singularity
    if (max_val < epsilon) {
      log("Warning: Matrix is nearly singular\n");
      return 0;
    }

    // Swap maximum row with current row
    if (max_row != i) {
      for (j = 0; j <= n; j++) {
        double temp = aug[i][j];
        aug[i][j] = aug[max_row][j];
        aug[max_row][j] = temp;
      }
    }

    // Eliminate column i
    for (j = i + 1; j < n; j++) {
      double factor = aug[j][i] / aug[i][i];
      for (k = i; k <= n; k++) {
        aug[j][k] -= factor * aug[i][k];
      }
    }
  }

  // Back substitution
  for (i = n - 1; i >= 0; i--) {
    if (fabs(aug[i][i]) < epsilon) {
      log("Warning: Zero pivot encountered\n");
      return 0;
    }

    x[i] = aug[i][n];
    for (j = i + 1; j < n; j++) {
      x[i] -= aug[i][j] * x[j];
    }
    x[i] /= aug[i][i];

    // Check for invalid results
    if (isnan(x[i]) || isinf(x[i])) {
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
Matrix3x3 *fcv_calculate_perspective_transform(
  Corners *src_corners,
  Corners *dst_corners
) {
  // Initialize matrices with zeros
  double A[8][8] = {{0}};
  double b[8] = {0};
  double x[8] = {0};

  // Identity matrix as fallback
  static Matrix3x3 identity = {1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0};

  if (!src_corners || !dst_corners) {
    log("Error: NULL pointer passed to calculate_perspective_transform\n");
    return &identity;
  }

#ifdef DEBUG_LOGGING
  fprintf(stderr, "[C] Calculating perspective transform:\n");
  fprintf(
    stderr,
    "src_corners:\ntl(%f, %f)\ntr(%f, %f)\nbr(%f, %f)\nbl(%f, %f)\n\n",
    src_corners->tl_x,
    src_corners->tl_y,
    src_corners->tr_x,
    src_corners->tr_y,
    src_corners->br_x,
    src_corners->br_y,
    src_corners->bl_x,
    src_corners->bl_y
  );
  fprintf(
    stderr,
    "dst_corners:\ntl(%f, %f)\ntr(%f, %f)\nbr(%f, %f)\nbl(%f, %f)\n\n",
    dst_corners->tl_x,
    dst_corners->tl_y,
    dst_corners->tr_x,
    dst_corners->tr_y,
    dst_corners->br_x,
    dst_corners->br_y,
    dst_corners->bl_x,
    dst_corners->bl_y
  );
#endif

  // Validate input coordinates
  if (isnan(src_corners->tl_x) || isnan(src_corners->tl_y) ||
      isnan(src_corners->tr_x) || isnan(src_corners->tr_y) ||
      isnan(src_corners->br_x) || isnan(src_corners->br_y) ||
      isnan(src_corners->bl_x) || isnan(src_corners->bl_y) ||
      isnan(dst_corners->tl_x) || isnan(dst_corners->tl_y) ||
      isnan(dst_corners->tr_x) || isnan(dst_corners->tr_y) ||
      isnan(dst_corners->br_x) || isnan(dst_corners->br_y) ||
      isnan(dst_corners->bl_x) || isnan(dst_corners->bl_y)) {
    log("Error: Invalid coordinates (NaN) detected\n");
    return &identity;
  }

  // Set up the system of equations
  for (int32_t i = 0; i < 4; i++) {
    double srcX = 0.0, srcY = 0.0, dstX = 0.0, dstY = 0.0;

    // Safely extract coordinates
    switch (i) {
    case 0: // Top-left
      srcX = src_corners->tl_x;
      srcY = src_corners->tl_y;
      dstX = dst_corners->tl_x;
      dstY = dst_corners->tl_y;
      break;
    case 1: // Top-right
      srcX = src_corners->tr_x;
      srcY = src_corners->tr_y;
      dstX = dst_corners->tr_x;
      dstY = dst_corners->tr_y;
      break;
    case 2: // Bottom-right
      srcX = src_corners->br_x;
      srcY = src_corners->br_y;
      dstX = dst_corners->br_x;
      dstY = dst_corners->br_y;
      break;
    case 3: // Bottom-left
      srcX = src_corners->bl_x;
      srcY = src_corners->bl_y;
      dstX = dst_corners->bl_x;
      dstY = dst_corners->bl_y;
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
    A[i + 4][3] = srcX;
    A[i + 4][4] = srcY;
    A[i + 4][5] = 1.0;
    A[i + 4][6] = -srcX * dstY;
    A[i + 4][7] = -srcY * dstY;
    b[i + 4] = dstY;
  }

  log("Solve the system of equations …\n");
  if (!solve_linear_system(A, b, x)) {
    log("Failed to solve system, returning identity matrix\n");
    return &identity;
  }

  // Validate solution
  for (int32_t i = 0; i < 8; i++) {
    if (isnan(x[i]) || isinf(x[i]) || fabs(x[i]) > 1e6) {
      log("Error: Invalid solution values detected\n");
      return &identity;
    }
  }

  Matrix3x3 *result = malloc(sizeof(Matrix3x3));
  *result = (Matrix3x3){x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], 1.0};

#ifdef DEBUG_LOGGING
  fprintf(stderr, "Result matrix:\n");
  fprintf(stderr, "%f, %f, %f\n", result->m00, result->m01, result->m02);
  fprintf(stderr, "%f, %f, %f\n", result->m10, result->m11, result->m12);
  fprintf(stderr, "%f, %f, %f\n", result->m20, result->m21, result->m22);
#endif

  // Final validation of the result matrix
  if (isnan(result->m00) || isnan(result->m01) || isnan(result->m02) ||
      isnan(result->m10) || isnan(result->m11) || isnan(result->m12) ||
      isnan(result->m20) || isnan(result->m21) || isnan(result->m22) ||
      isinf(result->m00) || isinf(result->m01) || isinf(result->m02) ||
      isinf(result->m10) || isinf(result->m11) || isinf(result->m12) ||
      isinf(result->m20) || isinf(result->m21) || isinf(result->m22)) {
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
uint8_t *fcv_apply_matrix_3x3(
  int32_t in_width,
  int32_t in_height,
  uint8_t *in_data,
  int32_t out_width,
  int32_t out_height,
  Matrix3x3 *tmat
) {
  // Patch flip matrix if needed
  if (fabs(tmat->m00 + 1.0) < 1e-9 && fabs(tmat->m11 + 1.0) < 1e-9 &&
      tmat->m02 == 0.0 && tmat->m12 == 0.0) {
    tmat->m02 = in_width - 1;
    tmat->m12 = in_height - 1;
  }

  uint8_t *out_data = calloc(out_width * out_height * 4, sizeof(uint8_t));

  if (!out_data) { // Memory allocation failed
    return NULL;
  }

  // Iterate through every pixel in the output image
  for (int32_t out_y = 0; out_y < out_height; ++out_y) {
    for (int32_t out_x = 0; out_x < out_width; ++out_x) {
      // Apply the inverse transformation to find the corresponding source pixel
      double w = tmat->m20 * out_x + tmat->m21 * out_y + tmat->m22;
      if (fabs(w) < 1e-10) {
        continue; // Skip if w is too close to zero
      }

      double srcX = (tmat->m00 * out_x + tmat->m01 * out_y + tmat->m02) / w;
      double srcY = (tmat->m10 * out_x + tmat->m11 * out_y + tmat->m12) / w;

      // Convert source coordinates to integers
      int32_t x0 = (int32_t)floor(srcX);
      int32_t y0 = (int32_t)floor(srcY);
      int32_t x1 = x0 + 1;
      int32_t y1 = y0 + 1;

      // Verify that the anchor pixel is inside the source image
      if (x0 >= 0 && x0 < in_width && y0 >= 0 && y0 < in_height) {

        // Clamp the neighbor coordinates so that a (degenerated)
        // bilinear interpolation can be applied at the image borders.
        int32_t x1c = (x1 < in_width) ? x1 : x0;
        int32_t y1c = (y1 < in_height) ? y1 : y0;

        double dx = srcX - x0;
        double dy = srcY - y0;

        // If a neighbour got clamped we force the corresponding weight to 0
        if (x1c == x0) {
          dx = 0.0;
        }
        if (y1c == y0) {
          dy = 0.0;
        }

        uint8_t *p00 = &in_data[(y0 * in_width + x0) * 4];
        uint8_t *p01 = &in_data[(y0 * in_width + x1c) * 4];
        uint8_t *p10 = &in_data[(y1c * in_width + x0) * 4];
        uint8_t *p11 = &in_data[(y1c * in_width + x1c) * 4];

        for (int32_t c = 0; c < 4; ++c) {
          out_data[(out_y * out_width + out_x) * 4 + c] =
            (uint8_t)(p00[c] * (1 - dx) * (1 - dy) + p01[c] * dx * (1 - dy) +
                      p10[c] * (1 - dx) * dy + p11[c] * dx * dy);
        }
      }
    }
  }

  return out_data;
}
// File: src/rgba_to_grayscale.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "rgba_to_grayscale.h"
#else
#include "flatcv.h"
#endif

/**
 * Convert raw RGBA row-major top-to-bottom image data
 * to a single channel grayscale image data.
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param data Pointer to the pixel data.
 * @return Pointer to the single channel grayscale image data.
 */
uint8_t *fcv_rgba_to_grayscale(
  uint32_t width,
  uint32_t height,
  uint8_t const *const data
) {
  uint32_t img_length_px = width * height;
  uint8_t *grayscale_data = malloc(img_length_px);

  if (!grayscale_data) { // Memory allocation failed
    return NULL;
  }

  // Process each pixel row by row
  for (uint32_t i = 0; i < width * height; i++) {
    uint32_t rgba_index = i * 4;

    uint8_t r = data[rgba_index];
    uint8_t g = data[rgba_index + 1];
    uint8_t b = data[rgba_index + 2];

    uint8_t gray = (r * R_WEIGHT + g * G_WEIGHT + b * B_WEIGHT) >> 8;

    grayscale_data[i] = gray;
  }

  return grayscale_data;
}
// File: src/single_to_multichannel.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "single_to_multichannel.h"
#else
#include "flatcv.h"
#endif

/**
 * Convert single channel grayscale image data to
 * RGBA row-major top-to-bottom image data.
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param data Pointer to the pixel data.
 */
uint8_t *fcv_single_to_multichannel(
  uint32_t width,
  uint32_t height,
  uint8_t const *const data
) {
  uint32_t img_length_px = width * height;
  uint8_t *multichannel_data = malloc(img_length_px * 4);

  if (!multichannel_data) { // Memory allocation failed
    return NULL;
  }

  for (uint32_t i = 0; i < img_length_px; i++) {
    uint32_t rgba_index = i * 4;
    multichannel_data[rgba_index] = data[i];
    multichannel_data[rgba_index + 1] = data[i];
    multichannel_data[rgba_index + 2] = data[i];
    multichannel_data[rgba_index + 3] = 255;
  }

  return multichannel_data;
}
// File: src/sobel_edge_detection.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "conversion.h"
#include "perspectivetransform.h"
#include "rgba_to_grayscale.h"
#include "sobel_edge_detection.h"
#else
#include "flatcv.h"
#endif

/**
 * Apply Sobel edge detection to the image data and return single-channel
 * grayscale data. Uses Sobel kernels to detect edges in horizontal and vertical
 * directions, then combines them to get the edge magnitude.
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param channels Number of channels in the input image.
 *                 (1: Grayscale, 3: RGB, 4: RGBA)
 * @param data Pointer to the input pixel data.
 * @return Pointer to the single-channel grayscale edge-detected image data.
 */
uint8_t *fcv_sobel_edge_detection(
  uint32_t width,
  uint32_t height,
  uint32_t channels,
  uint8_t const *const data
) {
  if (!data || width == 0 || height == 0) {
    return NULL;
  }

  uint32_t img_length_px = width * height;
  uint8_t *grayscale_data;
  bool allocated_grayscale = false;

  if (channels == 1) {
    // Single-channel input, use data directly
    grayscale_data = (uint8_t *)data;
    allocated_grayscale = false;
  }
  else {
    // Multi-channel input, convert to grayscale
    grayscale_data = fcv_rgba_to_grayscale(width, height, data);
    if (!grayscale_data) {
      return NULL;
    }
    allocated_grayscale = true;
  }

  uint8_t *sobel_data = malloc(img_length_px);
  if (!sobel_data) {
    if (allocated_grayscale) {
      free(grayscale_data);
    }
    return NULL;
  }

  // Sobel kernels
  int32_t sobel_x[3][3] = {{-1, 0, 1}, {-2, 0, 2}, {-1, 0, 1}};
  int32_t sobel_y[3][3] = {{-1, -2, -1}, {0, 0, 0}, {1, 2, 1}};

  // Temporary buffer to store magnitudes for normalization
  double *magnitudes = malloc(img_length_px * sizeof(double));
  if (!magnitudes) {
    if (allocated_grayscale) {
      free(grayscale_data);
    }
    free(sobel_data);
    return NULL;
  }

  double min_magnitude = INFINITY;
  double max_magnitude = 0.0;

  // First pass: calculate all magnitudes and find min/max
  for (uint32_t y = 0; y < height; y++) {
    for (uint32_t x = 0; x < width; x++) {
      int32_t gx = 0;
      int32_t gy = 0;

      // Apply Sobel kernels
      for (int32_t ky = -1; ky <= 1; ky++) {
        for (int32_t kx = -1; kx <= 1; kx++) {
          int32_t px = x + kx;
          int32_t py = y + ky;

          // Handle boundaries by using nearest pixel values
          if (px < 0) {
            px = 0;
          }
          if ((uint32_t)px >= width) {
            px = width - 1;
          }
          if (py < 0) {
            py = 0;
          }
          if ((uint32_t)py >= height) {
            py = height - 1;
          }

          uint8_t pixel = grayscale_data[py * width + px];
          gx += pixel * sobel_x[ky + 1][kx + 1];
          gy += pixel * sobel_y[ky + 1][kx + 1];
        }
      }

      double magnitude = sqrt(gx * gx + gy * gy);
      magnitudes[y * width + x] = magnitude;

      if (magnitude < min_magnitude) {
        min_magnitude = magnitude;
      }
      if (magnitude > max_magnitude) {
        max_magnitude = magnitude;
      }
    }
  }

  // Second pass: normalize and assign to output
  double range = max_magnitude - min_magnitude;
  if (range == 0.0) {
    range = 1.0; // Avoid division by zero
  }

  for (uint32_t y = 0; y < height; y++) {
    for (uint32_t x = 0; x < width; x++) {
      double magnitude = magnitudes[y * width + x];

      // Normalize to 0-255 range based on actual min/max
      int32_t final_magnitude =
        (int32_t)(((magnitude - min_magnitude) / range) * 255.0);

      sobel_data[y * width + x] = final_magnitude;
    }
  }

  free(magnitudes);

  if (allocated_grayscale) {
    free(grayscale_data);
  }
  return sobel_data;
}
// File: src/sort_corners.c
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef FLATCV_AMALGAMATION
#include "1_types.h"
#else
#include "flatcv.h"
#endif

typedef struct {
  double r;     // radius
  double theta; // angle
} PolarPoint;

// Helper function to convert Cartesian to polar coordinates
PolarPoint cartesian_to_polar(Point2D cart) {
  PolarPoint polar;
  polar.r = sqrt(cart.x * cart.x + cart.y * cart.y);
  double theta = atan2(cart.y, cart.x);

  // Normalize theta: Convert to degrees and apply normalization
  double theta_deg = theta * 180.0 / M_PI;
  if (theta_deg < 0) {
    polar.theta = -theta_deg;
  }
  else {
    polar.theta = -(theta_deg - 360.0);
  }

  return polar;
}

// Comparison function for qsort (sorting by angle in descending order)
int compare_polar_angles(const void *a, const void *b) {
  const PolarPoint *pa = (const PolarPoint *)a;
  const PolarPoint *pb = (const PolarPoint *)b;

  if (pa->theta < pb->theta) {
    return 1; // Descending order
  }
  if (pa->theta > pb->theta) {
    return -1;
  }
  return 0;
}

// Helper structure to keep track of original indices during sorting
typedef struct {
  PolarPoint polar;
  uint32_t original_index;
} IndexedPolarPoint;

int compare_indexed_polar_angles(const void *a, const void *b) {
  const IndexedPolarPoint *pa = (const IndexedPolarPoint *)a;
  const IndexedPolarPoint *pb = (const IndexedPolarPoint *)b;

  if (pa->polar.theta < pb->polar.theta) {
    return 1; // Descending order
  }
  if (pa->polar.theta > pb->polar.theta) {
    return -1;
  }
  return 0;
}

Corners sort_corners(
  uint32_t width,
  uint32_t height,
  uint32_t out_width,
  uint32_t out_height,
  Point2D *corners,
  uint32_t num_corners,
  Point2D *result
) {
  if (num_corners < 3) {
    // Not enough corners, return zeros
    Corners empty_corners = {0, 0, 0, 0, 0, 0, 0, 0};
    return empty_corners;
  }

  // Allocate memory for working corners (max of num_corners or 4 for 3-corner
  // case)
  uint32_t max_corners = (num_corners >= 4) ? num_corners : 4;
  Point2D *working_corners = malloc(max_corners * sizeof(Point2D));
  if (!working_corners) {
    Corners empty_corners = {0, 0, 0, 0, 0, 0, 0, 0};
    return empty_corners;
  }

  uint32_t corners_to_process;

  if (num_corners == 3) {
    // Copy the 3 corners we have
    working_corners[0] = corners[0];
    working_corners[1] = corners[1];
    working_corners[2] = corners[2];

    // Calculate the fourth corner to complete a parallelogram
    // For 3 corners A, B, C, we need to determine which corner is missing
    // and calculate it using vector addition

    // First, sort the 3 corners by position to understand the layout
    Point2D sorted[3] = {corners[0], corners[1], corners[2]};

    // Find bounding box of the 3 corners
    double min_x = sorted[0].x, max_x = sorted[0].x;
    double min_y = sorted[0].y, max_y = sorted[0].y;
    for (int i = 1; i < 3; i++) {
      if (sorted[i].x < min_x) {
        min_x = sorted[i].x;
      }
      if (sorted[i].x > max_x) {
        max_x = sorted[i].x;
      }
      if (sorted[i].y < min_y) {
        min_y = sorted[i].y;
      }
      if (sorted[i].y > max_y) {
        max_y = sorted[i].y;
      }
    }

    // Determine which corner is missing by finding which position in the
    // rectangle is empty
    bool has_tl = false, has_tr = false, has_bl = false, has_br = false;
    for (int i = 0; i < 3; i++) {
      double x = sorted[i].x;
      double y = sorted[i].y;

      // Use small tolerance for floating point comparison
      double tolerance = 1.0;

      if (fabs(x - min_x) < tolerance && fabs(y - min_y) < tolerance) {
        has_tl = true;
      }
      else if (fabs(x - max_x) < tolerance && fabs(y - min_y) < tolerance) {
        has_tr = true;
      }
      else if (fabs(x - min_x) < tolerance && fabs(y - max_y) < tolerance) {
        has_bl = true;
      }
      else if (fabs(x - max_x) < tolerance && fabs(y - max_y) < tolerance) {
        has_br = true;
      }
    }

    // Calculate the missing corner
    Point2D missing_corner;
    if (!has_tl) {
      missing_corner.x = min_x;
      missing_corner.y = min_y;
    }
    else if (!has_tr) {
      missing_corner.x = max_x;
      missing_corner.y = min_y;
    }
    else if (!has_bl) {
      missing_corner.x = min_x;
      missing_corner.y = max_y;
    }
    else if (!has_br) {
      missing_corner.x = max_x;
      missing_corner.y = max_y;
    }
    else {
      // Fallback: use parallelogram completion
      // For a parallelogram ABCD where we have A, B, C and need D:
      // D = A + C - B (assuming B and D are diagonal)
      // Try all combinations and pick the best one
      Point2D candidates[3];
      candidates[0].x = corners[0].x + corners[1].x - corners[2].x;
      candidates[0].y = corners[0].y + corners[1].y - corners[2].y;
      candidates[1].x = corners[0].x + corners[2].x - corners[1].x;
      candidates[1].y = corners[0].y + corners[2].y - corners[1].y;
      candidates[2].x = corners[1].x + corners[2].x - corners[0].x;
      candidates[2].y = corners[1].y + corners[2].y - corners[0].y;

      // Choose the candidate that gives the most reasonable result
      missing_corner = candidates[2]; // Default to the last one
      double best_score = -1;

      for (int i = 0; i < 3; i++) {
        double score = 0;
        if (candidates[i].x >= 0) {
          score += 1;
        }
        if (candidates[i].y >= 0) {
          score += 1;
        }

        // Check if it's not a duplicate
        bool is_duplicate = false;
        for (int j = 0; j < 3; j++) {
          if (fabs(candidates[i].x - corners[j].x) < 0.1 &&
              fabs(candidates[i].y - corners[j].y) < 0.1) {
            is_duplicate = true;
            break;
          }
        }
        if (!is_duplicate) {
          score += 5;
        }

        if (score > best_score) {
          best_score = score;
          missing_corner = candidates[i];
        }
      }
    }

    working_corners[3] = missing_corner;
    corners_to_process = 4;
  }
  else {
    // Copy all existing corners
    for (uint32_t i = 0; i < num_corners; i++) {
      working_corners[i] = corners[i];
    }
    corners_to_process = num_corners;
  }

  // Clockwise corner sorting starting from top-left corner
  // Works with any number of corners >= 3

  // Step 1: Find the center point (centroid)
  double center_x = 0.0, center_y = 0.0;
  for (uint32_t i = 0; i < corners_to_process; i++) {
    center_x += working_corners[i].x;
    center_y += working_corners[i].y;
  }
  center_x /= corners_to_process;
  center_y /= corners_to_process;

  // Step 2: Find the top-left corner (minimum x+y sum)
  uint32_t top_left_idx = 0;
  double min_sum = working_corners[0].x + working_corners[0].y;
  for (uint32_t i = 1; i < corners_to_process; i++) {
    double sum = working_corners[i].x + working_corners[i].y;
    if (sum < min_sum) {
      min_sum = sum;
      top_left_idx = i;
    }
  }

  // Step 3: Create array with indices and calculate angles relative to center
  typedef struct {
    uint32_t index;
    double angle;
    double distance;
  } CornerInfo;

  CornerInfo *corner_info = malloc(corners_to_process * sizeof(CornerInfo));
  if (!corner_info) {
    free(working_corners);
    Corners empty_corners = {0, 0, 0, 0, 0, 0, 0, 0};
    return empty_corners;
  }

  for (uint32_t i = 0; i < corners_to_process; i++) {
    corner_info[i].index = i;
    corner_info[i].distance = sqrt(
      (working_corners[i].x - center_x) * (working_corners[i].x - center_x) +
      (working_corners[i].y - center_y) * (working_corners[i].y - center_y)
    );

    // Calculate angle from center to corner
    double dx = working_corners[i].x - center_x;
    double dy = working_corners[i].y - center_y;
    corner_info[i].angle = atan2(dy, dx);

    // Normalize angle to [0, 2π) range
    if (corner_info[i].angle < 0) {
      corner_info[i].angle += 2.0 * M_PI;
    }
  }

  // Step 4: Calculate reference angle from center to top-left corner
  double top_left_angle = corner_info[top_left_idx].angle;

  // Step 5: Adjust all angles relative to top-left corner and sort clockwise
  for (uint32_t i = 0; i < corners_to_process; i++) {
    // Adjust angle relative to top-left corner
    corner_info[i].angle = corner_info[i].angle - top_left_angle;

    // Normalize to [0, 2π) range
    while (corner_info[i].angle < 0) {
      corner_info[i].angle += 2.0 * M_PI;
    }
    while (corner_info[i].angle >= 2.0 * M_PI) {
      corner_info[i].angle -= 2.0 * M_PI;
    }
  }

  // Step 6: Sort corners by angle (clockwise from top-left)
  for (uint32_t i = 0; i < corners_to_process - 1; i++) {
    for (uint32_t j = 0; j < corners_to_process - 1 - i; j++) {
      bool should_swap = false;

      if (corner_info[j].angle > corner_info[j + 1].angle) {
        should_swap = true;
      }
      else if (fabs(corner_info[j].angle - corner_info[j + 1].angle) < 1e-6) {
        // If angles are very close, sort by distance from center (closer first)
        if (corner_info[j].distance > corner_info[j + 1].distance) {
          should_swap = true;
        }
      }

      if (should_swap) {
        CornerInfo temp = corner_info[j];
        corner_info[j] = corner_info[j + 1];
        corner_info[j + 1] = temp;
      }
    }
  }

  // Step 7: Copy sorted corners to result
  for (uint32_t i = 0; i < corners_to_process; i++) {
    result[i] = working_corners[corner_info[i].index];
  }

  // Scale corners back to original image dimensions
  double scale_x = (double)width / out_width;
  double scale_y = (double)height / out_height;

  // Return corners struct using the sorted result
  Corners sorted_corners_result = {
    .tl_x = result[0].x * scale_x,
    .tl_y = result[0].y * scale_y,
    .tr_x = result[1].x * scale_x,
    .tr_y = result[1].y * scale_y,
    .br_x = result[2].x * scale_x,
    .br_y = result[2].y * scale_y,
    .bl_x = result[3].x * scale_x,
    .bl_y = result[3].y * scale_y
  };

  // Clean up allocated memory
  free(corner_info);
  free(working_corners);

  return sorted_corners_result;
}
// File: src/trim.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "crop.h"
#include "trim.h"
#else
#include "flatcv.h"
#endif

static bool
pixels_match(const uint8_t *pixel1, const uint8_t *pixel2, uint32_t channels) {
  for (uint32_t c = 0; c < channels; c++) {
    if (pixel1[c] != pixel2[c]) {
      return false;
    }
  }
  return true;
}

/**
 * Trim border pixels that have the same color.
 *
 * @param width Pointer to width of the image (will be updated).
 * @param height Pointer to height of the image (will be updated).
 * @param channels Number of channels in the image.
 * @param data Pointer to the pixel data.
 * @return Pointer to the new trimmed image data.
 */
uint8_t *fcv_trim(
  int32_t *width,
  int32_t *height,
  uint32_t channels,
  uint8_t const *const data
) {
  if (!data || !width || !height || *width <= 0 || *height <= 0) {
    return NULL;
  }

  uint32_t w = (uint32_t)*width;
  uint32_t h = (uint32_t)*height;
  uint32_t left = 0;
  uint32_t top = 0;
  uint32_t right = w;
  uint32_t bottom = h;

  // Trim from the left
  while (left < right) {
    const uint8_t *ref_pixel = &data[(top * w + left) * channels];
    bool can_trim = true;

    // Check if the entire left column has the same color as the reference pixel
    for (uint32_t y = top; y < bottom; y++) {
      const uint8_t *pixel = &data[(y * w + left) * channels];
      if (!pixels_match(pixel, ref_pixel, channels)) {
        can_trim = false;
        break;
      }
    }

    if (can_trim && right - left > 1) {
      left++;
    }
    else {
      break;
    }
  }

  // Trim from the right
  while (left < right) {
    const uint8_t *ref_pixel = &data[(top * w + (right - 1)) * channels];
    bool can_trim = true;

    // Check if the entire right column has the same color as the reference
    // pixel
    for (uint32_t y = top; y < bottom; y++) {
      const uint8_t *pixel = &data[(y * w + (right - 1)) * channels];
      if (!pixels_match(pixel, ref_pixel, channels)) {
        can_trim = false;
        break;
      }
    }

    if (can_trim && right - left > 1) {
      right--;
    }
    else {
      break;
    }
  }

  // Trim from the top
  while (top < bottom && bottom - top > 1) {
    const uint8_t *ref_pixel = &data[(top * w + left) * channels];
    bool can_trim = true;

    // Check if the entire top row has the same color as the reference pixel
    for (uint32_t x = left; x < right; x++) {
      const uint8_t *pixel = &data[(top * w + x) * channels];
      if (!pixels_match(pixel, ref_pixel, channels)) {
        can_trim = false;
        break;
      }
    }

    if (can_trim) {
      top++;
    }
    else {
      break;
    }
  }

  // Trim from the bottom
  while (top < bottom && bottom - top > 1) {
    const uint8_t *ref_pixel = &data[((bottom - 1) * w + left) * channels];
    bool can_trim = true;

    // Check if the entire bottom row has the same color as the reference pixel
    for (uint32_t x = left; x < right; x++) {
      const uint8_t *pixel = &data[((bottom - 1) * w + x) * channels];
      if (!pixels_match(pixel, ref_pixel, channels)) {
        can_trim = false;
        break;
      }
    }

    if (can_trim) {
      bottom--;
    }
    else {
      break;
    }
  }

  // Calculate new dimensions
  uint32_t new_width = right - left;
  uint32_t new_height = bottom - top;

  // If no trimming was done, return a copy
  if (left == 0 && top == 0 && right == w && bottom == h) {
    uint8_t *result = malloc(w * h * channels);
    if (result) {
      memcpy(result, data, w * h * channels);
    }
    return result;
  }

  // Use existing crop function to extract the trimmed region
  uint8_t *trimmed_data =
    fcv_crop(w, h, channels, data, left, top, new_width, new_height);

  // Update dimensions
  *width = (int32_t)new_width;
  *height = (int32_t)new_height;

  return trimmed_data;
}
// File: src/watershed_segmentation.c
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef FLATCV_AMALGAMATION
#include "1_types.h"
#include "watershed_segmentation.h"
#else
#include "flatcv.h"
#endif

typedef struct {
  int32_t x, y;
  int32_t label;
} QueueItem;

typedef struct {
  QueueItem *items;
  int32_t front;
  int32_t rear;
  int32_t size;
  int32_t capacity;
} Queue;

static Queue *create_queue(int32_t capacity) {
  Queue *q = malloc(sizeof(Queue));
  if (!q) {
    return NULL;
  }

  q->items = malloc(capacity * sizeof(QueueItem));
  if (!q->items) {
    free(q);
    return NULL;
  }

  q->front = 0;
  q->rear = -1;
  q->size = 0;
  q->capacity = capacity;
  return q;
}

static void destroy_queue(Queue *q) {
  if (q) {
    free(q->items);
    free(q);
  }
}

static int32_t is_queue_empty(Queue *q) { return q->size == 0; }

static int32_t enqueue(Queue *q, int32_t x, int32_t y, int32_t label) {
  if (q->size >= q->capacity) {
    return 0;
  }

  q->rear = (q->rear + 1) % q->capacity;
  q->items[q->rear].x = x;
  q->items[q->rear].y = y;
  q->items[q->rear].label = label;
  q->size++;
  return 1;
}

static QueueItem dequeue(Queue *q) {
  QueueItem item = {-1, -1, -1};
  if (q->size == 0) {
    return item;
  }

  item = q->items[q->front];
  q->front = (q->front + 1) % q->capacity;
  q->size--;
  return item;
}

/**
 * Watershed segmentation
 * using (x, y) coordinate markers with elevation-based flooding
 *
 * Implements the watershed transform for image segmentation by treating the
 * grayscale image as an elevation map. Water floods from the marker points,
 * and watershed lines form where different regions would meet. Lower
 * intensity values represent valleys where water accumulates, and higher
 * values represent hills/ridges.
 *
 * @param width Width of the image.
 * @param height Height of the image.
 * @param grayscale_data Pointer to the input grayscale image data.
 * @param markers Array of Point2D markers with x,y coordinates.
 * @param num_markers Number of markers in the array.
 * @return Pointer to the segmented image data.
 */
uint8_t *fcv_watershed_segmentation(
  uint32_t width,
  uint32_t height,
  uint8_t const *const grayscale_data,
  Point2D *markers,
  uint32_t num_markers,
  bool create_boundaries
) {
  if (!grayscale_data || !markers || num_markers == 0) {
    return NULL;
  }

  // Validate all marker positions
  for (uint32_t m = 0; m < num_markers; m++) {
    int32_t marker_x = (int32_t)markers[m].x;
    int32_t marker_y = (int32_t)markers[m].y;

    // Check bounds - if any marker is invalid, fail the entire operation
    if (marker_x < 0 || marker_x >= (int32_t)width || marker_y < 0 ||
        marker_y >= (int32_t)height) {
      return NULL;
    }
  }

  uint32_t img_length_px = width * height;

  // Create output data
  uint8_t *output_data = malloc(img_length_px * 4);
  if (!output_data) {
    return NULL;
  }

  // Initialize labels array (-1 = unvisited, 0+ = region labels)
  int32_t *labels = malloc(img_length_px * sizeof(int32_t));
  if (!labels) {
    free(output_data);
    return NULL;
  }

  // Initialize all pixels as unvisited
  for (uint32_t i = 0; i < img_length_px; i++) {
    labels[i] = -1;
  }

  // Create queue for flood fill
  Queue *queue = create_queue(img_length_px);
  if (!queue) {
    free(output_data);
    free(labels);
    return NULL;
  }

  // Initialize markers and add to queue
  for (uint32_t m = 0; m < num_markers; m++) {
    int32_t marker_x = (int32_t)markers[m].x;
    int32_t marker_y = (int32_t)markers[m].y;
    int32_t idx = marker_y * width + marker_x;
    int32_t label = (int32_t)m; // Label starts from 0
    labels[idx] = label;
    enqueue(queue, marker_x, marker_y, label);
  }

  // Level-wise watershed algorithm - process pixels in elevation order
  int32_t dx[] = {-1, 1, 0, 0};
  int32_t dy[] = {0, 0, -1, 1};

  // Process each elevation level from 0 to 255
  for (int32_t current_level = 0; current_level <= 255; current_level++) {
    // Single-step expansion: each region grows by exactly one pixel layer per
    // iteration
    bool changed = true;
    while (changed) {
      changed = false;

      // Find all pixels at current level that should be added in this step
      for (uint32_t y = 0; y < height; y++) {
        for (uint32_t x = 0; x < width; x++) {
          int32_t idx = y * width + x;

          // Skip if already labeled
          if (labels[idx] != -1) {
            continue;
          }

          // Allow water to flow downhill:
          // Process pixels at or below current level.
          // This enables proper watershed behavior
          // where water flows from high to low elevation.
          bool can_flood = (grayscale_data[idx] <= current_level);

          if (!can_flood) {
            continue;
          }

          // Check if this pixel neighbors any labeled pixel
          int32_t neighbor_label = -1;
          bool multiple_labels = false;

          for (int32_t d = 0; d < 4; d++) {
            int32_t nx = x + dx[d];
            int32_t ny = y + dy[d];

            if (nx >= 0 && nx < (int32_t)width && ny >= 0 &&
                ny < (int32_t)height) {
              int32_t neighbor_idx = ny * width + nx;
              if (labels[neighbor_idx] != -1) {
                if (neighbor_label == -1) {
                  neighbor_label = labels[neighbor_idx];
                }
                else if (neighbor_label != labels[neighbor_idx]) {
                  multiple_labels = true;
                  break;
                }
              }
            }
          }

          // If found exactly one labeled neighbor, mark for expansion
          // If multiple labels and boundaries disabled, use first found label
          if (neighbor_label != -1 &&
              (!multiple_labels || !create_boundaries)) {
            enqueue(queue, x, y, neighbor_label);
            changed = true;
          }
        }
      }

      // Apply all expansions from this step simultaneously
      while (!is_queue_empty(queue)) {
        QueueItem item = dequeue(queue);
        int32_t idx = item.y * width + item.x;
        labels[idx] = item.label;
      }
    }
  }

  destroy_queue(queue);

  // Generate distinct colors for each region
  uint8_t colors[10][3] = {
    {255, 0, 0},     // red
    {0, 255, 0},     // green
    {0, 0, 255},     // blue
    {255, 255, 0},   // yellow
    {255, 0, 255},   // magenta
    {0, 255, 255},   // cyan
    {255, 128, 0},   // orange
    {128, 0, 255},   // purple
    {255, 192, 203}, // pink
    {128, 128, 128}  // gray
  };

  // Create output image
  for (uint32_t i = 0; i < img_length_px; i++) {
    uint32_t rgba_idx = i * 4;
    int32_t label = labels[i];

    if (label == -1) {
      // Unassigned pixels -> background (black)
      output_data[rgba_idx] = 0;     // R
      output_data[rgba_idx + 1] = 0; // G
      output_data[rgba_idx + 2] = 0; // B
    }
    else if (label >= 0 && label < 10) {
      // Assigned to a region
      output_data[rgba_idx] = colors[label][0];     // R
      output_data[rgba_idx + 1] = colors[label][1]; // G
      output_data[rgba_idx + 2] = colors[label][2]; // B
    }
    else {
      // Fallback for more than 10 regions
      output_data[rgba_idx] = 128;     // R
      output_data[rgba_idx + 1] = 128; // G
      output_data[rgba_idx + 2] = 128; // B
    }
    output_data[rgba_idx + 3] = 255; // A
  }

  free(labels);

  return output_data;
}
/* End of FlatCV amalgamation */
