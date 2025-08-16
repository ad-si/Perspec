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

  // Free int32_termediate result
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
 * Resize an image by given resize factors using bilinear int32_terpolation.
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

          double int32_terpolated = p00 * (1 - dx) * (1 - dy) +
                                    p01 * dx * (1 - dy) + p10 * (1 - dx) * dy +
                                    p11 * dx * dy;

          resized_data[(out_y * *out_width + out_x) * 4 + c] =
            (uint8_t)(int32_terpolated + 0.5);
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
#include "watershed_segmentation.h"
#else
#include "flatcv.h"
#endif

// #define DEBUG_LOGGING

/** Count number of distinct colors in RGBA image
 * 1. Convert to grayscale
 * 2. Scale to 256x256 (save scale ratio for x and y)
 * 3. Blur image
 * 4. Create elevation map with Sobel
 * 5. Flatten elevation map at center seed to avoid being trapped in a local
 * minimum
 * 6. `watershed(image=elevation_map, markers=markers)`
 *     Set center as marker for foreground basin and border for background basin
 * 7. Check region count equals 2
 * 8. Smooth result
 *     1. TODO: Add border to avoid connection with image boundaries
 *     1. Perform binary closing
 *     1. TODO: Remove border
 * 9. Use Foerstner corner detector (Harris detector corners are shifted
 * inwards)
 * 10. TODO: Sort corners
 * 11. TODO: Select 4 corners with the largest angle while maint32_taining their
 * order
 * 12. Normalize corners based on scale ratio
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
  write_debug_img(out_img, "temp_2_resized_image.png");
#endif

  // 3. Apply Gaussian blur
  uint8_t const *blurred_image =
    fcv_apply_gaussian_blur(out_width, out_height, 3.0, resized_image);
  // Don't free resized_image here, as it is used for debugging later
  if (!blurred_image) {
    fprintf(stderr, "Error: Failed to apply Gaussian blur\n");
    exit(EXIT_FAILURE);
  }

  // 5. Create elevation map with Sobel edge detection
  uint8_t *elevation_map = (uint8_t *)
    fcv_sobel_edge_detection(out_width, out_height, 4, blurred_image);
  free((void *)blurred_image);
  if (!elevation_map) {
    fprintf(stderr, "Error: Failed to create elevation map with Sobel\n");
    exit(EXIT_FAILURE);
  }

  // 6. Flatten elevation map at center to not get trapped in a local minimum
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

#ifdef DEBUG_LOGGING
  out_img.data = elevation_map;
  out_img.channels = 1; // Grayscale
  write_debug_img(out_img, "temp_6_elevation_map.png");
#endif

  // 7. Perform watershed segmentation
  int32_t num_markers = 2;
  Point2D *markers = malloc(num_markers * sizeof(Point2D));
  if (!markers) {
    fprintf(stderr, "Error: Failed to allocate memory for markers\n");
    free((void *)elevation_map);
    exit(EXIT_FAILURE);
  }
  // Set center as foreground marker and upper left corner as background marker
  markers[0] = (Point2D){.x = out_width / 2.0, .y = out_height / 2.0};
  markers[1] = (Point2D){.x = 0, .y = 0};

  uint8_t *segmented_image = fcv_watershed_segmentation(
    out_width,
    out_height,
    elevation_map,
    markers,
    num_markers,
    false // No boundaries
  );
  free((void *)elevation_map);
  free((void *)markers);

#ifdef DEBUG_LOGGING
  out_img.data = segmented_image;
  out_img.channels = 4;
  write_debug_img(out_img, "temp_7_watershed.png");
#endif

  // Check if the image has exactly 2 regions (foreground and background)
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
  write_debug_img(out_img, "temp_7_watershed_binary.png");
#endif

  // 8. Smooth the result
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
  write_debug_img(out_img, "temp_8_segmented_closed.png");
#endif

  // 9. Find corners in the closed image
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
  write_debug_img(out_img, "temp_9_corner_response.png");
#endif
  free(w_channel);

  // 10. Find corner peaks using thresholds
  CornerPeaks *peaks = fcv_corner_peaks(
    out_width,
    out_height,
    corner_response,
    16,  // Minimum distance between peaks
    0.5, // accuracy_thresh
    0.3  // roundness_thresh
  );
  free((void *)corner_response);
  if (!peaks) {
    fprintf(stderr, "Error: Failed to find corner peaks\n");
    exit(EXIT_FAILURE);
  }

#ifdef DEBUG_LOGGING
  // Draw corner peaks on the resized image
  for (int32_t i = 0; i < peaks->count; i++) {
    fcv_draw_circle(
      out_width,
      out_height,
      4,        // RGBA
      "FF0000", // Red color for corners
      2,        // Radius
      peaks->points[i].x,
      peaks->points[i].y,
      (uint8_t *)resized_image
    );
  }

  out_img.data = resized_image;
  out_img.channels = 4;
  write_debug_img(out_img, "temp_10_corners.png");
#endif
  free((void *)resized_image);

  Corners corners = {
    .tl_x = peaks->points[0].x,
    .tl_y = peaks->points[0].y,
    .tr_x = peaks->points[1].x,
    .tr_y = peaks->points[1].y,
    .br_x = peaks->points[2].x,
    .br_y = peaks->points[2].y,
    .bl_x = peaks->points[3].x,
    .bl_y = peaks->points[3].y
  };

  return corners;
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
 * Helper function to draw 8 symmetric point32_ts of a circle
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

  // Initial point32_ts
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
#define log(msg) print32_tf("DEBUG: %s\n", msg)
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
  print32_tf("[C] Calculating perspective transform:\n");
  print32_tf(
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
  print32_tf(
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
  print32_tf("Result matrix:\n");
  print32_tf("%f, %f, %f\n", result->m00, result->m01, result->m02);
  print32_tf("%f, %f, %f\n", result->m10, result->m11, result->m12);
  print32_tf("%f, %f, %f\n", result->m20, result->m21, result->m22);
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
 * Use bilinear int32_terpolation to calculate final pixel values.
 */
uint8_t *fcv_apply_matrix_3x3(
  int32_t in_width,
  int32_t in_height,
  uint8_t *in_data,
  int32_t out_width,
  int32_t out_height,
  Matrix3x3 *tmat
) {
#ifdef DEBUG_LOGGING
  print32_tf("Input data:\n");
  for (int32_t i = 0; i < in_width; i++) {
    for (int32_t j = 0; j < in_height; j++) {
      print32_tf("%d ", in_data[(i * in_width + j) * 4]);
    }
    print32_tf("\n");
  }
#endif

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

      // Convert source coordinates to int32_tegers
      int32_t x0 = (int32_t)floor(srcX);
      int32_t y0 = (int32_t)floor(srcY);
      int32_t x1 = x0 + 1;
      int32_t y1 = y0 + 1;

      // Verify that the anchor pixel is inside the source image
      if (x0 >= 0 && x0 < in_width && y0 >= 0 && y0 < in_height) {

        // Clamp the neighbor coordinates so that a (degenerated)
        // bilinear int32_terpolation can be applied at the image borders.
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

#ifdef DEBUG_LOGGING
  print32_tf("Output data:\n");
  for (int32_t i = 0; i < out_width; i++) {
    for (int32_t j = 0; j < out_height; j++) {
      print32_tf("%d ", out_data[(i * out_width + j) * 4]);
    }
    print32_tf("\n");
  }
#endif

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
 * grayscale image as an elevation map. Water floods from the marker point32_ts,
 * and watershed lines form where different regions would meet. Lower
 * int32_tensity values represent valleys where water accumulates, and higher
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
