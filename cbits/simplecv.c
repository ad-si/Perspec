#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

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
  * @param data Pointer to the pixel data.
  * @return Pointer to the monochrome image data.
  */
unsigned char const * const otsu_threshold_rgba(
  unsigned int width,
  unsigned int height,
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

  apply_global_threshold(img_length_px, grayscale_img, optimal_threshold);

  unsigned char const * const monochrome_data = single_to_multichannel(
    width,
    height,
    grayscale_img
  );

  free(grayscale_img);

  return monochrome_data;
}
