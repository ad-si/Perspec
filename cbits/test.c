#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "simplecv.h"
#include "perspectivetransform.h"


int test_otsu_threshold () {
  unsigned int width = 4;
  unsigned int height = 4;
  unsigned char data[64] = {
    1,1,1,255, 2,2,2,255, 9,9,9,255, 8,8,8,255,
    2,2,2,255, 1,1,1,255, 9,9,9,255, 7,7,7,255,
    2,2,2,255, 0,0,0,255, 8,8,8,255, 2,2,2,255,
    0,0,0,255, 2,2,2,255, 9,9,9,255, 8,8,8,255
  };

  unsigned char const * const monochrome_data = otsu_threshold_rgba(width, height, data);

  unsigned char expected_data[64] = {
    0,0,0,255, 0,0,0,255, 255,255,255,255, 255,255,255,255,
    0,0,0,255, 0,0,0,255, 255,255,255,255, 255,255,255,255,
    0,0,0,255, 0,0,0,255, 255,255,255,255, 0,0,0,255,
    0,0,0,255, 0,0,0,255, 255,255,255,255, 255,255,255,255
  };

  bool test_passed = true;

  for (unsigned int i = 0; i < 64; i++) {
    if (monochrome_data[i] != expected_data[i]) {
      test_passed = false;
      printf(
        "Test failed at index %d: Monochrome data %d != Expected data %d\n",
        i,
        monochrome_data[i],
        expected_data[i]
      );
      // return 1;
    }
  }

  free((void*)monochrome_data);

  if (test_passed) {
    printf("✅ Otsu's threshold test passed\n");
    return 0;
  }
  else {
    printf("❌ Test failed\n");
    return 1;
  }
}


int test_perspective_transform() {
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

  printf("✅ Perspective transform test passed\n");
  return 0;
}


int main () {
  if (
    !test_otsu_threshold() &&
    !test_perspective_transform()
  ) {
    printf("✅ All tests passed\n");
    return 0;
  }
  else {
    printf("❌ Some tests failed\n");
    return 1;
  }
}
