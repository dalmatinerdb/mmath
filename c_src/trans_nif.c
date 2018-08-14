#include "erl_nif.h"
#include "mmath.h"

#include <math.h>

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
  return 0;
}

#define MATHF(name, fun)                                                \
  static ERL_NIF_TERM                                                   \
  name (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])            \
  {                                                                     \
    ErlNifBinary bin;                                                   \
    ERL_NIF_TERM r;                                                     \
    ffloat* vs;                                                         \
    ErlNifSInt64 int_v;                                                 \
    double m;                                                           \
    ffloat* target;                                                     \
    int count;                                                          \
                                                                        \
    if (argc != 2)                                                      \
      return enif_make_badarg(env);                                     \
                                                                        \
    GET_BIN(0, bin, count, vs);                                         \
                                                                        \
    if (enif_get_int64(env, argv[1], &int_v)) {                         \
      m = (double) int_v;                                               \
    } else if (!enif_get_double(env, argv[1], &m)) {                    \
      return enif_make_badarg(env);                                     \
    }                                                                   \
                                                                        \
    if (! (target = (ffloat*) enif_make_new_binary(env, count * sizeof(ffloat), &r))) \
      return enif_make_badarg(env);                                     \
                                                                        \
    for (int i = 0; i < count; i++) {                                   \
      target[i] = fun (vs[i], m);                                       \
    }                                                                   \
    return r;                                                           \
  }

MATHF(add, float_addc)
MATHF(sub, float_subc)
MATHF(mul, float_mulc)
MATHF(divide, float_divc)

MATHF(min, float_minc)
MATHF(max, float_maxc)



static ERL_NIF_TERM
derivate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  ffloat* target;
  ffloat* vs;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, bin, count, vs);

  if (! (target = (ffloat*) enif_make_new_binary(env, count * sizeof(ffloat), &r)))
    return enif_make_badarg(env); // TODO return propper error

  if (count == 0)
    return r;
  if (count == 1) {
    target[0] = (ffloat) {.value = 0, .confidence = 0};
    return r;
  }
  for (int i = 1; i < count; i++) {
    target[i] = float_sub(vs[i], vs[i-1]);
  }
  target[0] = target[1];
  target[0].confidence = 0;
  return r;
}

static ERL_NIF_TERM
confidence(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ffloat* vs;
  ffloat* target;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (! (target = (ffloat*) enif_make_new_binary(env, count * sizeof(ffloat), &r)))
    return enif_make_badarg(env); // TODO return propper error
  for (int i = 0; i < count; i++) {
    target[i] = (ffloat){
      .value = vs[i].confidence,
      .confidence = CERTAIN
    };
  }
  return r;
}

/* Given series, confidence level and default value reset everything
   equal to or below with the given default value.
 */
static ERL_NIF_TERM
replace_below_confidence(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ffloat* vs;
  ErlNifSInt64 int_v;
  double threshold_confidence;
  ErlNifSInt64 int_default;
  double default_value;
  ffloat* target;
  int count;

  if (argc != 3)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (enif_get_int64(env, argv[1], &int_v)) {
    threshold_confidence = (double) int_v;
  } else if (!enif_get_double(env, argv[1], &threshold_confidence)) {
    return enif_make_badarg(env);
  }

  if (enif_get_int64(env, argv[2], &int_default)) {
    default_value = (double) int_default;
  } else if (!enif_get_double(env, argv[2], &default_value)) {
    return enif_make_badarg(env);
  }

  if (! (target = (ffloat*) enif_make_new_binary(env, count * sizeof(ffloat), &r)))
    return enif_make_badarg(env); // TODO return propper error
  for (int i = 0; i < count; i++) {
    if (vs[i].confidence > threshold_confidence) {
      target[i] = (ffloat){
        .value = vs[i].value,
        .confidence = vs[i].confidence
      };
    } else {
      target[i] = (ffloat){
        .value = default_value,
	.confidence = vs[i].confidence
      };
    }
  }
  return r;
}

static ERL_NIF_TERM
square_root(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ffloat* vs;
  ffloat* target;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (! (target = (ffloat*) enif_make_new_binary(env, count * sizeof(ffloat), &r)))
    return enif_make_badarg(env); // TODO return propper error
  for (int i = 0; i < count; i++) {
    if (vs[i].value < 0) {
      target[i] = (ffloat){
        .confidence = vs[i].confidence,
        .value = sqrt(vs[i].value * -1) * - 1
      };
    } else {
      target[i] = (ffloat){
        .confidence = vs[i].confidence,
        .value = sqrt(vs[i].value)
      };
    }
  }
  return r;
}

static ERL_NIF_TERM
c_log10(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ffloat* vs;
  ffloat* target;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (! (target = (ffloat*) enif_make_new_binary(env, count * sizeof(ffloat), &r)))
    return enif_make_badarg(env); // TODO return propper error
  for (int i = 0; i < count; i++) {
    if (vs[i].value < 0) {
      target[i] = (ffloat){
        .confidence = vs[i].confidence,
        .value = log10(vs[i].value * -1) * - 1
      };
    } else if (vs[i].value == 0) {
      target[i] = (ffloat){
        .confidence = vs[i].confidence,
        .value = 0
      };
    } else {
      target[i] = (ffloat){
        .confidence = vs[i].confidence,
        .value = log10(vs[i].value)
      };
    }
  }
  return r;
}

static ERL_NIF_TERM
c_abs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ffloat* vs;
  ffloat* target;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (! (target = (ffloat*) enif_make_new_binary(env, count * sizeof(ffloat), &r)))
    return enif_make_badarg(env); // TODO return propper error
  for (int i = 0; i < count; i++) {
    if (vs[i].value >= 0) {
      target[i] = vs[i];
    } else {
      target[i] = (ffloat){
        .confidence = vs[i].confidence,
        .value = vs[i].value * - 1.0
      };
    }
  }
  return r;
}


static ErlNifFunc nif_funcs[] = {
  {"add",         2, add},
  {"sub",         2, sub},
  {"mul",         2, mul},
  {"min",         2, min},
  {"max",         2, max},
  {"divide",      2, divide},
  {"derivate",    1, derivate},
  {"confidence",  1, confidence},
  {"replace_below_confidence",  3, replace_below_confidence},
  {"sqrt_scale",  1, square_root},
  {"log10_scale", 1, c_log10},
  {"abs",         1, c_abs}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(mmath_trans, nif_funcs, &load, NULL, &upgrade, NULL);
