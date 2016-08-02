#include "erl_nif.h"
#include "mmath.h"


inline ffloat
float_mul(ffloat v, double m) {
  return (ffloat){
    .value = v.value * m,
      .confidence = v.confidence
      };
}

inline ffloat
float_div(ffloat v, double m) {
  return (ffloat){
    .value = v.value / m,
      .confidence = v.confidence
      };
}

inline ffloat
float_add(ffloat a, ffloat b) {
  double confidence = (a.confidence + b.confidence) / 2;
  return (ffloat) {
    .value = a.value + b.value,
      .confidence = confidence
      };
}
inline ffloat
float_add3(ffloat a, ffloat b, ffloat c) {
  double confidence = (a.confidence + b.confidence + a.confidence) / 3;
  return (ffloat) {
    .value = a.value + b.value + c.value,
      .confidence = confidence
      };
}

inline ffloat /* a - b */
float_sub(ffloat a, ffloat b) {
  double confidence = (a.confidence + b.confidence) / 2;
  return (ffloat) {
    .value = a.value - b.value,
      .confidence = confidence
      };
}

inline ffloat
float_neg(ffloat a) {
  return (ffloat) {
    .value = -a.value,
      .confidence = a.confidence
      };
}

ffloat float_min(ffloat a, ffloat b) {
  /* if (b.confidence == 0) { */
  /*   return a; */
  /* } */
  /* if (a.confidence == 0) { */
  /*   return b; */
  /* } */
  if (b.value < a.value) {
    return b;
  } else {
    return a;
  }
}


ffloat float_min3(ffloat a, ffloat b, ffloat c) {
  return float_min(a, float_min(b, c));
}


ffloat float_max(ffloat a, ffloat b) {
  /* if (b.confidence == 0) { */
  /*   return a; */
  /* } */
  /* if (a.confidence == 0) { */
  /*   return b; */
  /* } */
  if (b.value > a.value) {
    return b;
  } else {
    return a;
  }
}


ffloat float_max3(ffloat a, ffloat b, ffloat c) {
  return float_max(a, float_max(b, c));
}
