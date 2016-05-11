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

static ERL_NIF_TERM
mul(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  decimal* vs;
  ErlNifSInt64 m;
  decimal* target;
  int count;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_BIN(0, bin, count, vs);

  if (!enif_get_int64(env, argv[1], &m))
    return enif_make_badarg(env);

  if (! (target = (decimal*) enif_make_new_binary(env, count * sizeof(decimal), &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    target[i] = dec_mul(vs[i], m);
  }
  return r;
}

static ERL_NIF_TERM
divide(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  decimal* vs;
  ErlNifSInt64 m;
  decimal* target;
  int count;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_BIN(0, bin, count, vs);

  if (!enif_get_int64(env, argv[1], &m))
    return enif_make_badarg(env);

  if (!m)
    return enif_make_badarg(env);

  if (! (target = (decimal*) enif_make_new_binary(env, count * sizeof(decimal), &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    target[i] = dec_div(vs[i], m);
  }
  return r;
}

static ERL_NIF_TERM
derivate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  decimal* target;
  decimal* vs;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, bin, count, vs);

  if (count < 1) // can't be empty
    return enif_make_badarg(env);

  if (! (target = (decimal*) enif_make_new_binary(env, (count - 1) * sizeof(decimal), &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 1; i < count; i++) {
    target[i - 1] = dec_sub(vs[i], vs[i-1]);
  }
  return r;
}

static ERL_NIF_TERM
confidence(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  decimal* vs;
  decimal* target;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (! (target = (decimal*) enif_make_new_binary(env, count * sizeof(decimal), &r)))
    return enif_make_badarg(env); // TODO return propper error
  for (int i = 0; i < count; i++) {
    if (vs[i].confidence != 0)
      target[i] = (decimal){
        .coefficient = vs[i].confidence,
        .exponent = -3,
        .confidence = CERTAIN
      };
    else
      target[i] = (decimal){
        .coefficient = 0,
        .exponent = 0,
        .confidence = CERTAIN
      };
  }
  return r;
}

static ErlNifFunc nif_funcs[] = {
  {"mul",        2, mul},
  {"divide",     2, divide},
  {"derivate",   1, derivate},
  {"confidence", 1, confidence}

};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(mmath_trans, nif_funcs, &load, NULL, &upgrade, NULL);
