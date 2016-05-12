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
to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM *acc;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  ffloat last = {.confidence = 0};
  unsigned count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  acc = malloc(count * sizeof(ERL_NIF_TERM));


  for (unsigned i = 0 ; i < count; i++) {
    if (IS_SET(vs[i]))
      last = float_deserialize(vs[i]);
    // Maybe we should use bitstring representation for floats,
    // because it don't loose precision
    acc[i] = enif_make_double(env, last.value);
  }
  r = enif_make_list_from_array(env, acc, count);
  free(acc);
  return r;
}

static ERL_NIF_TERM
from_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM list;
  ERL_NIF_TERM cell;
  ErlNifSInt64 int_v;
  double float_v;
  ErlNifBinary bin_v;
  ErlNifSInt64* target;
  ERL_NIF_TERM r;
  unsigned count;
  ffloat d;

  if (argc != 1)
    return enif_make_badarg(env);

  list = argv[0];

  if (!enif_get_list_length(env, list, &count))
    return enif_make_badarg(env);

  if (!(target = (ErlNifSInt64*) enif_make_new_binary(env, count * sizeof(ErlNifSInt64), &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (! enif_get_list_cell(env, list, &cell, &list))
      return enif_make_badarg(env); // TODO return propper error
    if (enif_get_int64(env, cell, &int_v)) {
      d = float_from_int64(int_v);
    } else if (enif_get_double(env, cell, &float_v))
      d = float_from_double(float_v);
    else
      return enif_make_badarg(env);
    target[i] = float_serialize(d);
  }
  return r;
}

static ERL_NIF_TERM
rdatasize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM list;
  ErlNifBinary bin;
  int count;

  if (argc != 0)
    return enif_make_badarg(env);

  return enif_make_int(env, sizeof(ffloat));
}

static ERL_NIF_TERM
realize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  ffloat* target;
  ffloat last = {.value = 0, .confidence = 0};
  int has_last = 0;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (! (target = (ffloat*) enif_make_new_binary(env, count * sizeof(ffloat), &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    last.confidence = 0;
    if (IS_SET(vs[i])) {
      last = FROM_DDB(vs[i]);
      if (! has_last) {
        for (int j = 0; j < i; j++) {
          target[j] = last;
          target[j].confidence = 0;
        }
        has_last = 1;
      }
    };
    target[i] = last;
  }
  return r;
}

static ERL_NIF_TERM
derealize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ffloat* vs;
  ErlNifSInt64* target;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, count * sizeof(ErlNifSInt64), &r)))
    return enif_make_badarg(env); // TODO return propper error
  for (int i = 0; i < count; i++) {
    target[i] = TO_DDB(vs[i]);
  }
  return r;
}

static ErlNifFunc nif_funcs[] = {
  {"from_list",    1, from_list},
  {"to_list",      1, to_list},
  {"rdatasize",    0, rdatasize},
  {"realize",      1, realize},
  {"derealize",    1, derealize}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(mmath_bin, nif_funcs, &load, NULL, &upgrade, NULL);
