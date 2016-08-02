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

// SUM

static ERL_NIF_TERM
sum2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ErlNifBinary b;
  ERL_NIF_TERM r;
  ffloat* vs_a;
  ffloat* vs_b;
  ffloat* target;
  ffloat last_a = {0, 0};
  ffloat last_b = {0, 0};

  int count_a;
  int count_b;
  int count;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_BIN(0, a, count_a, vs_a);
  GET_BIN(1, b, count_b, vs_b);
  count = count_a > count_b ? count_a : count_b;

  target_size = count * sizeof(ffloat);
  if (! (target = (ffloat*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  if (count_a == count_b) {
    for (int i = 0; i < count; i++) {
      target[i] = float_add(vs_a[i], vs_b[i]);
    }
  } else {
    for (int i = 0; i < count; i++) {
      // If we have a valid A or B for this opint
      // we copy it in, otherwise we reuse the
      // prior one and set confidence to 0.
      if (i < count_a) {
        last_a = vs_a[i];
      } else {
        last_a.confidence = 0;
      }
      if (i < count_b) {
        last_b = vs_b[i];
      } else {
        last_b.confidence = 0;
      }

      // if neither A nor B are set here we keep a blank
      target[i] = float_add(last_a, last_b);
    }
  }
  return r;
}

static ERL_NIF_TERM
sum3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ErlNifBinary b;
  ErlNifBinary c;
  ERL_NIF_TERM r;
  ffloat* vs_a;
  ffloat* vs_b;
  ffloat* vs_c;
  ffloat* target;
  ffloat last_a = {0, 0};
  ffloat last_b = {0, 0};
  ffloat last_c = {0, 0};

  int count_a;
  int count_b;
  int count_c;
  int count;
  int target_size;

  if (argc != 3)
    return enif_make_badarg(env);

  GET_BIN(0, a, count_a, vs_a);
  GET_BIN(1, b, count_b, vs_b);
  GET_BIN(2, c, count_c, vs_c);

  count = count_a > count_b ? count_a : count_b;
  count = count > count_c ? count : count_c;

  target_size = count * sizeof(ffloat);
  if (! (target = (ffloat*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  if (count_a == count_b && count_b == count_c) {
    for (int i = 0; i < count; i++) {
      target[i] = float_add3(vs_a[i], vs_b[i], vs_c[i]);
    }
  } else {
    for (int i = 0; i < count; i++) {
      if (i < count_a) {
        last_a = vs_a[i];
      } else {
        last_a.confidence = 0;
      }
      if (i < count_b){
        last_b = vs_b[i];
      } else {
        last_b.confidence = 0;
      }
      if (i < count_c){
        last_c = vs_c[i];
      } else {
        last_c.confidence = 0;
      }
      target[i] = float_add3(last_a, last_b, last_c);
    }
  }
  return r;
}

// MIN

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

static ERL_NIF_TERM
min2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ErlNifBinary b;
  ERL_NIF_TERM r;
  ffloat* vs_a;
  ffloat* vs_b;
  ffloat* target;
  ffloat last_a = {0, 0};
  ffloat last_b = {0, 0};

  int count_a;
  int count_b;
  int count;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_BIN(0, a, count_a, vs_a);
  GET_BIN(1, b, count_b, vs_b);
  count = count_a > count_b ? count_a : count_b;

  target_size = count * sizeof(ffloat);
  if (! (target = (ffloat*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  if (count_a == count_b) {
    for (int i = 0; i < count; i++) {
      target[i] = float_min(vs_a[i], vs_b[i]);
    }
  } else {
    for (int i = 0; i < count; i++) {
      // If we have a valid A or B for this opint
      // we copy it in, otherwise we reuse the
      // prior one and set confidence to 0.
      if (i < count_a) {
        last_a = vs_a[i];
      } else {
        last_a.confidence = 0;
      }
      if (i < count_b) {
        last_b = vs_b[i];
      } else {
        last_b.confidence = 0;
      }
      target[i] = float_min(last_a, last_b);
    }
  }
  return r;
}

static ERL_NIF_TERM
min3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ErlNifBinary b;
  ErlNifBinary c;
  ERL_NIF_TERM r;
  ffloat* vs_a;
  ffloat* vs_b;
  ffloat* vs_c;
  ffloat* target;
  ffloat last_a = {0, 0};
  ffloat last_b = {0, 0};
  ffloat last_c = {0, 0};

  int count_a;
  int count_b;
  int count_c;
  int count;
  int target_size;

  if (argc != 3)
    return enif_make_badarg(env);

  GET_BIN(0, a, count_a, vs_a);
  GET_BIN(1, b, count_b, vs_b);
  GET_BIN(2, c, count_c, vs_c);

  count = count_a > count_b ? count_a : count_b;
  count = count > count_c ? count : count_c;

  target_size = count * sizeof(ffloat);
  if (! (target = (ffloat*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  if (count_a == count_b && count_b == count_c) {
    for (int i = 0; i < count; i++) {
      target[i] = float_min3(vs_a[i], vs_b[i], vs_c[i]);
    }
  } else {
    for (int i = 0; i < count; i++) {
      if (i < count_a) {
        last_a = vs_a[i];
      } else {
        last_a.confidence = 0;
      }
      if (i < count_b){
        last_b = vs_b[i];
      } else {
        last_b.confidence = 0;
      }
      if (i < count_c){
        last_c = vs_c[i];
      } else {
        last_c.confidence = 0;
      }
      target[i] = float_min3(last_a, last_b, last_c);
    }
  }
  return r;
}

static ErlNifFunc nif_funcs[] = {
  {"sum",      2, sum2},
  {"sum",      3, sum3},
  {"min_",      2, min2},
  {"min_",      3, min3}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(mmath_comb, nif_funcs, &load, NULL, &upgrade, NULL);
