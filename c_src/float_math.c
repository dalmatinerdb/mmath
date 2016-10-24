#include "erl_nif.h"
#include "mmath.h"


inline ffloat
float_mulc(ffloat v, double m) {
  return (ffloat){
    .value = v.value * m,
      .confidence = v.confidence
      };
}

inline ffloat
float_divc(ffloat v, double m) {
  return (ffloat){
    .value = v.value / m,
      .confidence = v.confidence
      };
}

inline ffloat
float_addc(ffloat v, double m) {
  return (ffloat){
    .value = v.value + m,
      .confidence = v.confidence
      };
}

inline ffloat
float_subc(ffloat v, double m) {
  return (ffloat){
    .value = v.value - m,
      .confidence = v.confidence
      };
}

ffloat
float_mul(ffloat a, ffloat b) {
  return (ffloat){
    .value = a.value * b.value,
      .confidence = (a.confidence + b.confidence) / 2
      };
}

ffloat
float_mul3(ffloat a, ffloat b, ffloat c) {
  return (ffloat){
    .value = a.value * b.value * c.value,
      .confidence = (a.confidence + b.confidence + c.confidence) / 3
      };
}

ffloat
float_div(ffloat a, ffloat b) {
  double q = b.value;
  if (q == 0) {
    q = 1;
  }
  return (ffloat){
    .value = a.value / q,
      .confidence = (a.confidence + b.confidence) / 2
      };
}

ffloat
float_div3(ffloat a, ffloat b, ffloat c) {
  double q1 = b.value;
  double q2 = c.value;
  if (q1 == 0) {
    q1 = 1;
  }
  if (q2 == 0) {
    q2 = 1;
  }
  return (ffloat){
    .value = a.value / q1 / q2,
      .confidence = (a.confidence + b.confidence + c.confidence) / 3
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

inline ffloat /* a - b */
float_sub3(ffloat a, ffloat b, ffloat c) {
  double confidence = (a.confidence + b.confidence + c.confidence) / 3;
  return (ffloat) {
    .value = a.value - b.value - c.value,
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

inline ffloat
float_const(ffloat a, double b) {
   return a;
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
