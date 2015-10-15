#include "erl_nif.h"
#include "mmath.h"

#include <math.h>

#define COEFFICIENT_DIGITS 14
#define COEFFICIENT_BITS 48

#define TYPE_MASK = 0x00000000000000FFLL;
// #define DEC_MASK =


ddb_number
dec_to_ddb_number(dec v) {
  return htonll((! v.exponent) ?
                ((v.coefficient & 0x00FFFFFFFFFFFFFFLL) | 0x0100000000000000LL) :
                (v.coefficient & 0x0000FFFFFFFFFFFFLL) |
                ((long long)(v.exponent & 0xFF) << COEFFICIENT_BITS) |
                0x0200000000000000LL
                );
}

dec
dec_from_int64(ErlNifSInt64 v) {
  dec d = {.exponent = 0, .coefficient = v};
  return d;
}

// Very inefficient conversion which is loosing precisions.
// It is almost always preferable to read value from decimal strings
dec
dec_from_double(double v) {
  dec d;
  int sign = 1;

  if (v < 0) {
    sign = -1;
    v *= -1;
  }           
  d.exponent = (int)ceil(log10(v)) - COEFFICIENT_DIGITS;
  d.coefficient = (long long)(v / pow(10, d.exponent)) * sign;
  return d;
}
