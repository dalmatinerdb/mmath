#include "erl_nif.h"
#include "mmath.h"

#include <math.h>

#define COEFFICIENT_DIGITS 14
#define COEFFICIENT_BITS 48

#define TYPE_MASK        0xFF00000000000000LL
#define VALUE_MASK       0x00FFFFFFFFFFFFFFLL
#define EXPONENT_MASK    0x00FF000000000000LL
#define COEFFICIENT_MASK 0x0000FFFFFFFFFFFFLL

#define EMPTY_TYPE   0
#define INTEGER_TYPE 1
#define DECIMAL_TYPE 2


ErlNifSInt64
dec_serialize(decimal v) {
  return htonll((! v.exponent) ?
                ((v.coefficient & 0x00FFFFFFFFFFFFFFLL) | 0x0100000000000000LL) :
                (v.coefficient & 0x0000FFFFFFFFFFFFLL) |
                ((long long)(v.exponent & 0xFF) << COEFFICIENT_BITS) |
                0x0200000000000000LL
                );
}

decimal
dec_deserialize(ErlNifSInt64 ev) {
  decimal d;
  long long v = ntohll(ev);
  char type = (char)(v & TYPE_MASK);

  if (type == INTEGER_TYPE) {
    d.exponent = 0;
    d.coefficient = v & VALUE_MASK;
  } else {
    d.exponent = (signed char)((v & EXPONENT_MASK) >> COEFFICIENT_BITS);
    d.coefficient = v & COEFFICIENT_MASK;
  }
  return d;
}


decimal
dec_from_int64(ErlNifSInt64 v) {
  decimal d = {.exponent = 0, .coefficient = v};
  return d;
}

// Very inefficient conversion which is loosing precisions.
// It is almost always preferable to read value from decimal strings
decimal
dec_from_double(double v) {
  decimal d;
  int sign = 1;

  if (v < 0) {
    sign = -1;
    v *= -1;
  }           
  d.exponent = (int)ceil(log10(v)) - COEFFICIENT_DIGITS;
  d.coefficient = (long long)(v / pow(10, d.exponent)) * sign;
  return d;
}
