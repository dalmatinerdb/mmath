#include "erl_nif.h"
#include "mmath.h"

#define MAX_DIGITS 14
#define MAX_COEFFICIENT (long)1e+14

/**
 * TODO: switch to int64_t and uint64_t
 */

/* 
  Fast 64bit integer log10 (It returns floor of actual float)
  WARNING: calling ilog10c(0) yields undefined behaviour!

  THANKS TO COURTESY OF CAFXX, https://gist.github.com/CAFxX/ad150f2403a0604e14cc
*/
static uint8_t
qlog10(uint64_t v) {
  static const uint64_t thr[64] = {
    10000000000000000000ULL, 0, 0, 0, 1000000000000000000ULL, 0, 0, 100000000000000000ULL, 0, 0,
       10000000000000000ULL, 0, 0, 0,    1000000000000000ULL, 0, 0,    100000000000000ULL, 0, 0,
          10000000000000ULL, 0, 0, 0,       1000000000000ULL, 0, 0,       100000000000ULL, 0, 0,
             10000000000ULL, 0, 0, 0,          1000000000ULL, 0, 0,          100000000ULL, 0, 0,
                10000000ULL, 0, 0, 0,             1000000ULL, 0, 0,             100000ULL, 0, 0,
                   10000ULL, 0, 0, 0,                1000ULL, 0, 0,                100ULL, 0, 0,
                      10ULL, 0, 0, 0
  };
  uint32_t lz = __builtin_clzll(v);
  return (63 - lz) * 3 / 10 + (v >= thr[lz]);
}

/* Fast integer pow10. It make sense only for positive numbers, in range 0-19 */
static uint64_t
qipow10(uint8_t v) {
  static const uint64_t p[20] = {
    1, 10ULL, 100ULL, 1000ULL, 10000ULL, 100000ULL, 1000000ULL, 10000000ULL, 100000000ULL,
    1000000000ULL, 10000000000ULL, 100000000000ULL, 1000000000000ULL, 10000000000000ULL,
    100000000000000ULL, 1000000000000000ULL, 10000000000000000ULL, 100000000000000000ULL,
    1000000000000000000ULL, 10000000000000000000ULL
  };
  return p[v];
}

static inline void
dec_reduce(decimal *v) {
  int8_t d;
  int64_t c = llabs(v->coefficient);
  if (c > MAX_COEFFICIENT) {
    if (c < (MAX_COEFFICIENT * 10)) {
      v->coefficient /= 10;
      v->exponent += 1;
    } else {
      d = qlog10(c / MAX_COEFFICIENT) + 1;
      v->coefficient /= (int64_t)qipow10(d);
      v->exponent += d;
    }
  }
}

static inline void
dec_inflate(decimal *v) {
  int64_t c = llabs(v->coefficient);
  if(c == 0) return;
  // We take some big steps first, to speed things up a little
  // might not even be needed
  // I'm sure this can be mathed away.
  while ((c * 10000) < MAX_COEFFICIENT) {
    v->coefficient *= 10000;
    v->exponent -= 4;
    c *= 10000;
  }
  while ((c * 10) < MAX_COEFFICIENT) {
    v->coefficient *= 10;
    v->exponent -= 1;
    c *= 10;
  }
}

decimal
dec_mul(decimal v, int64_t m) {
  // TODO fix in situation when m is so big, that it will overflow coefficient
  decimal r = {
    .coefficient = v.coefficient * m,
    .exponent = v.exponent
  };
  dec_reduce(&r);
  return r;
}

decimal
dec_div(decimal v, int64_t m) {
  // TODO: Improve accuracy by adjusting coefficient before dividing
  dec_inflate(&v);
  decimal r = {
    .coefficient = v.coefficient / m,
    .exponent = v.exponent
  };
  dec_reduce(&r);
  return r;
}

static decimal
dec_add_aligned(int64_t a_coef, int64_t b_coef, int8_t e) {
  decimal r = {
    .coefficient = a_coef + b_coef,
    .exponent = e
  };
  dec_reduce(&r);
  return r;
}

/* Add decimal b to a. Decimal a has always bigger coefficient */
static decimal
dec_add_not_aligned(decimal big, decimal small) {
  int8_t over_digits = 0;
  int8_t e, de;
  int64_t a_coef, b_coef;

  if (big.coefficient == 0) {
    return small;
  }
  if (small.coefficient == 0) {
    return big;
  }

  e = small.exponent;
  de = big.exponent - e;
  b_coef = small.coefficient;
  over_digits = qlog10(labs(big.coefficient)) + de - MAX_DIGITS - 1;

  if (over_digits > 0) {
    e += over_digits;
    de -= over_digits;
    b_coef /= (int64_t)qipow10(over_digits);
  }

  a_coef = big.coefficient * qipow10(de);
  return dec_add_aligned(a_coef, b_coef, e);
}

inline decimal
dec_add(decimal a, decimal b) {
  if (a.exponent == b.exponent) {
    return dec_add_aligned(a.coefficient, b.coefficient, a.exponent);
  } else {
    if (a.exponent >= b.exponent) {
      return dec_add_not_aligned(a, b);
    } else {
      return dec_add_not_aligned(b, a);
    }
  }
}

inline decimal
dec_add3(decimal a, decimal b, decimal c) {
  return dec_add(dec_add(a, b), c);
}

inline decimal /* a - b */
dec_sub(decimal a, decimal b) {
  return dec_add(a, dec_neg(b));
}

inline decimal
dec_neg(decimal a) {
  return (decimal) {
    .coefficient = - a.coefficient,
    .exponent = a.exponent
  };
}

/* return 1 when 1 > b, 0 when equal and -1 otherwise */
int
dec_cmp(decimal a, decimal b) {
  //TODO: optimise, not going into actual computation
  decimal r = dec_sub(a, b);
  if (r.coefficient == 0 && r.exponent == 0) {
    return 0;
  } else if (r.coefficient < 0) {
    return -1;
  } else {
    return 1;
  }
}
