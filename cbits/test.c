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

  bool test_ok = true;

  for (unsigned int i = 0; i < 64; i++) {
    if (monochrome_data[i] != expected_data[i]) {
      test_ok = false;
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

  if (test_ok) {
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
    100, 100,  // Top-left
    400, 150,  // Top-right
    380, 400,  // Bottom-right
    120, 380   // Bottom-left
  };

  Corners dst = {
    0, 0,      // Top-left
    300, 0,    // Top-right
    300, 300,  // Bottom-right
    0, 300     // Bottom-left
  };

  Matrix3x3* tmat = calculate_perspective_transform(&src, &dst);

  double eps = 0.001;
  bool test_ok = true;

  if(fabs(tmat->m00 -  0.85256062) > eps){ printf("m00: %f\n", tmat->m00 ); test_ok = false;}
  if(fabs(tmat->m01 +  0.06089719) > eps){ printf("m01: %f\n", tmat->m01 ); test_ok = false;}
  if(fabs(tmat->m02 + 79.16634335) > eps){ printf("m02: %f\n", tmat->m02 ); test_ok = false;}
  if(fabs(tmat->m10 +  0.14503146) > eps){ printf("m10: %f\n", tmat->m10 ); test_ok = false;}
  if(fabs(tmat->m11 -  0.87018875) > eps){ printf("m11: %f\n", tmat->m11 ); test_ok = false;}
  if(fabs(tmat->m12 + 72.51572949) > eps){ printf("m12: %f\n", tmat->m12 ); test_ok = false;}
  if(fabs(tmat->m20 +  0.00022582) > eps){ printf("m20: %f\n", tmat->m20 ); test_ok = false;}
  if(fabs(tmat->m21 +  0.00044841) > eps){ printf("m21: %f\n", tmat->m21 ); test_ok = false;}
  if(fabs(tmat->m22 -  1) > eps){ printf("m22: %f\n", tmat->m22 ); test_ok = false;}

  if (test_ok) {
    printf("✅ Perspective transform test passed\n");
    return 0;
  }
  else {
    printf("❌ Perspective transform test failed\n");
    return 1;
  }
}

int test_perspective_transform_float() {
  Corners src = {
    278.44, 182.23,  // Top-left
    1251.25, 178.79,  // Top-right
    1395.63, 718.48,  // Bottom-right
    216.56, 770.04   // Bottom-left
  };

  Corners dst = {
    0, 0,      // Top-left
    1076.5, 0,    // Top-right
    1076.5, 574.86,  // Bottom-right
    0, 574.86     // Bottom-left
  };

  Matrix3x3 *tmat = calculate_perspective_transform(&src, &dst);

  double eps = 0.001;
  bool test_ok = true;

  if(fabs(tmat->m00 -   1.08707) > eps){ printf("m00: %f\n", tmat->m00 ); test_ok = false;}
  if(fabs(tmat->m01 -   0.114438) > eps){ printf("m01: %f\n", tmat->m01 ); test_ok = false;}
  if(fabs(tmat->m02 + 323.538) > eps){ printf("m02: %f\n", tmat->m02 ); test_ok = false;}
  if(fabs(tmat->m10 -   0.00445981) > eps){ printf("m10: %f\n", tmat->m10 ); test_ok = false;}
  if(fabs(tmat->m11 -   1.26121) > eps){ printf("m11: %f\n", tmat->m11 ); test_ok = false;}
  if(fabs(tmat->m12 + 231.072) > eps){ printf("m12: %f\n", tmat->m12 ); test_ok = false;}
  if(fabs(tmat->m20 +   0.0000708899) > eps){ printf("m20: %f\n", tmat->m20 ); test_ok = false;}
  if(fabs(tmat->m21 -   0.000395421) > eps){ printf("m21: %f\n", tmat->m21 ); test_ok = false;}
  if(fabs(tmat->m22 -   1) > eps){ printf("m22: %f\n", tmat->m22 ); test_ok = false;}

  if (test_ok) {
    printf("✅ Perspective transform with floats test passed\n");
    return 0;
  }
  else {
    printf("❌ Perspective transform with floats test failed\n");
    return 1;
  }
}


int main () {
  if (
    !test_otsu_threshold() &&
    !test_perspective_transform() &&
    !test_perspective_transform_float()
  ) {
    printf("✅ All tests passed\n");
    return 0;
  }
  else {
    printf("❌ Some tests failed\n");
    return 1;
  }
}
