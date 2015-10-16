#include "erl_nif.h"
#include "mmath.h"


dec
dec_mul(dec v, long m) {
  // TODO fix in situation when m is so big, that it will overflow coefficient
  dec r = {
    .coefficient = v.coefficient * m,
    .exponent = v.exponent
  };
  while (r.coefficient >= 99999999999999) {
    r.coefficient /= 10;
    r.exponent += 1;
  }
  return r;
}

dec
dec_div(dec v, long m) {
  // TODO: Improve accuracy by normalizing coefficient before dividing
  dec r = {
    .coefficient = v.coefficient / m,
    .exponent = v.exponent
  };
  while ((r.coefficient <= 99999999999999) && (r.exponent != 0)) {
    r.coefficient *= 10;
    r.exponent -= 1;
  }
  return r;
}
