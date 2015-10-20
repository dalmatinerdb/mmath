#include "erl_nif.h"
#include "mmath.h"


decimal
dec_mul(decimal v, long long m) {
  // TODO fix in situation when m is so big, that it will overflow coefficient
  decimal r = {
    .coefficient = v.coefficient * m,
    .exponent = v.exponent
  };
  while (r.coefficient >= 99999999999999) {
    r.coefficient /= 10;
    r.exponent += 1;
  }
  return r;
}

decimal
dec_div(decimal v, long long m) {
  // TODO: Improve accuracy by normalizing coefficient before dividing
  decimal r = {
    .coefficient = v.coefficient / m,
    .exponent = v.exponent
  };
  while ((r.coefficient <= 99999999999999) && (r.exponent != 0)) {
    r.coefficient *= 10;
    r.exponent -= 1;
  }
  return r;
}

decimal
dec_add(decimal a, decimal b) {
  // TODO: Implement properly taking into account expontnet
  decimal r = {
    .coefficient = a.coefficient + b.coefficient,
    .exponent = a.exponent
  };
  return r;
}

decimal
dec_add3(decimal a, decimal b, decimal c) {
  // TOOD: Make more efficient
  return dec_add(dec_add(a, b), c);
}

decimal
dec_sub(decimal a, decimal b) {
  // TODO: Implement properly taking into account expontnet
  decimal r = {
    .coefficient = a.coefficient - b.coefficient,
    .exponent = a.exponent
  };
  return r;
}

int
dec_cmp(decimal a, decimal b) {
  //TODO: optimise, not going into actual computation
  decimal r = dec_sub(b, a);
  if (r.coefficient == 0 && r.exponent == 0) {
    return 0;
  } else if (r.coefficient < 0) {
    return -1;
  } else {
    return 1;
  }
}
