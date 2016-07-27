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
