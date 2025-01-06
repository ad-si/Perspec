#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "simplecv.h"
#include "perspectivetransform.h"


int translate_matrix_test() {
  printf("🎬 Start translate matrix test …\n");
  // Flip both axes
  Matrix3x3 tmat = {
    1,  0,  -2,
    0,  1,  -2,
    0,  0,  1
  };

  int width = 4;
  int height = 4;
  unsigned char input_data[64] = {
    1,1,1,255, 7,7,7,255, 0,0,0,255, 0,0,0,255,
    2,2,2,255, 3,3,3,255, 0,0,0,255, 0,0,0,255,
    0,0,0,255, 0,0,0,255, 0,0,0,255, 0,0,0,255,
    0,0,0,255, 0,0,0,255, 0,0,0,255, 0,0,0,255
  };
  unsigned char expected_data[64] = {
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 1,1,1,255, 7,7,7,255,
    0,0,0,0, 0,0,0,0, 2,2,2,255, 3,3,3,255
  };

  // Apply transformation
  unsigned char* output_data = apply_matrix_3x3(
    width, height, input_data,
    width, height,
    &tmat
  );

  // Compare the output_data with the expected data
  for (int i = 0; i < width * height * 4; i++) {
    if (output_data[i] != expected_data[i]) {
      printf(
        "Mismatch at index %d: Expected: %u, Got: %u\n",
        i,
        expected_data[i],
        output_data[i]
      );
      return 1;
    }
  }

  free(output_data);

  return 0;
}


int flip_matrix_test() {
  printf("🎬 Start flip matrix test …\n");
  // Flip both axes
  Matrix3x3 tmat = {
   -1,  0,  0,
    0, -1,  0,
    0,  0,  1
  };

  int width = 4;
  int height = 4;
  unsigned char input_data[64] = {
    1,1,1,255, 2,2,2,255, 9,9,9,255, 8,8,8,255,
    2,2,2,255, 1,1,1,255, 9,9,9,255, 7,7,7,255,
    2,2,2,255, 0,0,0,255, 8,8,8,255, 2,2,2,255,
    0,0,0,255, 2,2,2,255, 9,9,9,255, 8,8,8,255
  };
  unsigned char expected_data[64] = {
    8,8,8,255, 9,9,9,255, 2,2,2,255, 0,0,0,255,
    2,2,2,255, 8,8,8,255, 0,0,0,255, 2,2,2,255,
    7,7,7,255, 9,9,9,255, 1,1,1,255, 2,2,2,255,
    8,8,8,255, 9,9,9,255, 2,2,2,255, 1,1,1,255
  };

  // Apply transformation
  unsigned char* output_data = apply_matrix_3x3(
    width, height, input_data,
    width, height,
    &tmat
  );

  // Compare the output_data with the expected data
  for (int i = 0; i < width * height * 4; i++) {
    if (output_data[i] != expected_data[i]) {
      printf(
        "Mismatch at index %d: Expected: %u, Got: %u\n",
        i,
        expected_data[i],
        output_data[i]
      );
      return 1;
    }
  }

  free(output_data);

  return 0;
}


int scale_matrix_test() {
  printf("🎬 Start scale matrix test …\n");
  // Scale image by 50%
  Matrix3x3 tmat = {
    2, 0  ,  0,
    0  , 2,  0,
    0  , 0  ,  1
  };

  int in_width = 4;
  int in_height = 4;
  unsigned char input_data[64] = {
    1,1,1,255, 1,1,1,255, 9,9,9,255, 9,9,9,255,
    1,1,1,255, 1,1,1,255, 9,9,9,255, 9,9,9,255,
    2,2,2,255, 2,2,2,255, 6,6,6,255, 6,6,6,255,
    2,2,2,255, 2,2,2,255, 6,6,6,255, 6,6,6,255,
  };

  int out_width = 4;
  int out_height = 4;
  unsigned char expected_data[64] = {
    1,1,1,255, 9,9,9,255, 0,0,0,0, 0,0,0,0,
    2,2,2,255, 6,6,6,255, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
  };

  // Apply transformation
  unsigned char* output_data = apply_matrix_3x3(
    in_width, in_height, input_data,
    out_width, out_height,
    &tmat
  );

  // Compare the output_data with the expected data
  for (int i = 0; i < out_width * out_height * 4; i++) {
    if (output_data[i] != expected_data[i]) {
      printf(
        "Mismatch at index %d: %u != %u\n",
        i, output_data[i], expected_data[i]
      );
      return 1;
    }
  }

  free(output_data);

  return 0;
}


int main () {
  if (
    !translate_matrix_test() &&
    !flip_matrix_test() &&
    !scale_matrix_test()
  ) {
    printf("✅ All tests passed\n");
    return 0;
  }
  else {
    printf("❌ Some tests failed\n");
    return 1;
  }
}
