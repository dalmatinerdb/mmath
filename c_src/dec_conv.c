#include "erl_nif.h"
#include "mmath.h"

#include <math.h>

#define COEFFICIENT_DIGITS 14
#define COEFFICIENT_BITS 48

#define TYPE_MASK        0xFF00000000000000LL
#define VALUE_MASK       0x00FFFFFFFFFFFFFFLL
#define VALUE_SIGN_MASK  0x0080000000000000LL
#define EXPONENT_MASK    0x00FF000000000000LL
#define COEFFICIENT_MASK 0x0000FFFFFFFFFFFFLL
#define COEFFICIENT_SIGN_MASK 0x0000800000000000LL

#define EMPTY_TYPE   0
#define INTEGER_TYPE 0x01
#define DECIMAL_TYPE 0x02


static double
qpow10(int8_t v) {
  static const double t[256] = {
    1.0, 1e+1, 1e+2, 1e+3, 1e+4, 1e+5, 1e+6, 1e+7, 1e+8, 1e+9,
    1e+10, 1e+11, 1e+12, 1e+13, 1e+14, 1e+15, 1e+16, 1e+17, 1e+18, 1e+19,
    1e+20, 1e+21, 1e+22, 1e+23, 1e+24, 1e+25, 1e+26, 1e+27, 1e+28, 1e+29,
    1e+30, 1e+31, 1e+32, 1e+33, 1e+34, 1e+35, 1e+36, 1e+37, 1e+38, 1e+39,
    1e+40, 1e+41, 1e+42, 1e+43, 1e+44, 1e+45, 1e+46, 1e+47, 1e+48, 1e+49,
    1e+50, 1e+51, 1e+52, 1e+53, 1e+54, 1e+55, 1e+56, 1e+57, 1e+58, 1e+59,
    1e+60, 1e+61, 1e+62, 1e+63, 1e+64, 1e+65, 1e+66, 1e+67, 1e+68, 1e+69,
    1e+70, 1e+71, 1e+72, 1e+73, 1e+74, 1e+75, 1e+76, 1e+77, 1e+78, 1e+79,
    1e+80, 1e+81, 1e+82, 1e+83, 1e+84, 1e+85, 1e+86, 1e+87, 1e+88, 1e+89,
    1e+90, 1e+91, 1e+92, 1e+93, 1e+94, 1e+95, 1e+96, 1e+97, 1e+98, 1e+99,
    1e+100, 1e+101, 1e+102, 1e+103, 1e+104, 1e+105, 1e+106, 1e+107, 1e+108, 1e+109,
    1e+110, 1e+111, 1e+112, 1e+113, 1e+114, 1e+115, 1e+116, 1e+117, 1e+118, 1e+119,
    1e+120, 1e+121, 1e+122, 1e+123, 1e+124, 1e+125, 1e+126, 1e+127,
    1e-128, 1e-127, 1e-126, 1e-125, 1e-124, 1e-123, 1e-122, 1e-121, 1e-120,
    1e-119, 1e-118, 1e-117, 1e-116, 1e-115, 1e-114, 1e-113, 1e-112, 1e-111, 1e-110,
    1e-109, 1e-108, 1e-107, 1e-106, 1e-105, 1e-104, 1e-103, 1e-102, 1e-101, 1e-100,
    1e-99, 1e-98, 1e-97, 1e-96, 1e-95, 1e-94, 1e-93, 1e-92, 1e-91, 1e-90,
    1e-89, 1e-88, 1e-87, 1e-86, 1e-85, 1e-84, 1e-83, 1e-82, 1e-81, 1e-80,
    1e-79, 1e-78, 1e-77, 1e-76, 1e-75, 1e-74, 1e-73, 1e-72, 1e-71, 1e-70,
    1e-69, 1e-68, 1e-67, 1e-66, 1e-65, 1e-64, 1e-63, 1e-62, 1e-61, 1e-60,
    1e-59, 1e-58, 1e-57, 1e-56, 1e-55, 1e-54, 1e-53, 1e-52, 1e-51, 1e-50,
    1e-49, 1e-48, 1e-47, 1e-46, 1e-45, 1e-44, 1e-43, 1e-42, 1e-41, 1e-40,
    1e-39, 1e-38, 1e-37, 1e-36, 1e-35, 1e-34, 1e-33, 1e-32, 1e-31, 1e-30,
    1e-29, 1e-28, 1e-27, 1e-26, 1e-25, 1e-24, 1e-23, 1e-22, 1e-21, 1e-20,
    1e-19, 1e-18, 1e-17, 1e-16, 1e-15, 1e-14, 1e-13, 1e-12, 1e-11, 1e-10,
    1e-09, 1e-08, 1e-07, 1e-06, 1e-05, 1e-04, 1e-03, 1e-02, 1e-01
  };
  return t[(uint8_t)v];
}

ErlNifSInt64
dec_serialize(decimal v) {
  return htonll((! v.exponent) ?
                ((v.coefficient & 0x00FFFFFFFFFFFFFFLL) | 0x0100000000000000LL) :
                (v.coefficient & 0x0000FFFFFFFFFFFFLL) |
                ((int64_t)(v.exponent & 0xFF) << COEFFICIENT_BITS) |
                0x0200000000000000LL
                );
}

decimal
dec_deserialize(ErlNifSInt64 ev) {
  decimal d;
  int64_t v = ntohll(ev);
  char type = (uint8_t)((v & TYPE_MASK) >> 56);

  if (type == INTEGER_TYPE) {
    d.exponent = 0;
    d.coefficient = v & VALUE_MASK;
    if (v & VALUE_SIGN_MASK) {
      d.coefficient |= ((int64_t) -1) & ~ VALUE_MASK;
    }
  } else if (type == DECIMAL_TYPE) {
    d.exponent = (int8_t)((v & EXPONENT_MASK) >> COEFFICIENT_BITS);
    d.coefficient = v & COEFFICIENT_MASK;
    if (v & COEFFICIENT_SIGN_MASK) {
      d.coefficient |= ((int64_t) -1) & ~ COEFFICIENT_MASK;
    }
  }
  return d;
}


decimal
dec_from_int64(int64_t v) {
  decimal d = {.exponent = 0, .coefficient = v};
  return d;
}

// Very inefficient conversion which is loosing precision.
// It is almost always preferable to read value from decimal strings
decimal
dec_from_double(double v) {
  decimal d;
  int sign = 1;

  if (v < 0) {
    sign = -1;
    v *= -1;
  }           
  d.exponent = (int8_t)ceil(log10(v)) - COEFFICIENT_DIGITS;
  d.coefficient = (int64_t)(v / qpow10(d.exponent)) * sign;
  return d;
}

decimal
dec_from_binary(int len, char* v) {
  decimal d = {.exponent = 0, .coefficient = 0};
  char seen_point = 0;
  char digits = 0;
  char c, x;

  for (int i = 0; i < len; i++) {
    c = v[i];
    if ((i == 0) && (c == '-' || c == '+'))
      continue;
    else if (c >= '0' && c <= '9') {
      x = c - '0';
      digits += 1;
    }
    else if (!seen_point && (c == '.'))
      seen_point = 1;
    // TODO: add scientific notataion support
    // TODO: report error on invalid character

    // TODO: keep extra precision if integer oveflows coefficient,
    //       but still falls into integer boundries
    if (digits <= COEFFICIENT_DIGITS) {
      d.coefficient = d.coefficient * 10 + x;
      if (seen_point)
        d.exponent -= 1;
    }
    else if (!seen_point)
      d.exponent += 1;
  }
  return d;
}

int64_t dec_to_int64(decimal v) {
  return v.coefficient * qpow10(v.exponent);
}

// Not advised conversion, that will loose precision
double dec_to_double(decimal v) {
  return (double)v.coefficient * qpow10(v.exponent);
}

// TODO: add to and from string reading
