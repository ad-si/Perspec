#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "simplecv.h"
#include "perspectivetransform.h"

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
