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
  decimal last = {.coefficient = 0, .exponent = 0};
  unsigned count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  acc = malloc(count * sizeof(ERL_NIF_TERM));


  for (unsigned i = 0 ; i < count; i++) {
    if (IS_SET(vs[i]))
      last = dec_deserialize(vs[i]);
    if (last.exponent >= 0)
      acc[i] = enif_make_int64(env, dec_to_int64(last));
    else
      // Maybe use bitstring representation for floats that do not loose precision
      acc[i] = enif_make_double(env, dec_to_double(last));
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
  decimal d;

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
    if (enif_get_int64(env, cell, &int_v))
      d = dec_from_int64(int_v);
    else if (enif_get_double(env, cell, &float_v))
      d = dec_from_double(float_v);
    else if (enif_inspect_binary(env, cell, &bin_v) ||
             enif_inspect_iolist_as_binary(env, cell, &bin_v))
      d = dec_from_binary(bin_v.size, bin_v.data);
    else
      return enif_make_badarg(env);
    target[i] = dec_serialize(d);
  }
  return r;
}

static ERL_NIF_TERM
realize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  decimal* target;
  decimal last = {.coefficient = 0, .exponent = 0};
  int has_last = 0;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (! (target = (decimal*) enif_make_new_binary(env, count * sizeof(decimal), &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (IS_SET(vs[i])) {
      last = FROM_DDB(vs[i]);
      if (! has_last) {
        for (int j = 0; j < i; j++) {
          target[j] = last;
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
  decimal* vs;
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
  {"realize",      1, realize},
  {"derealize",    1, derealize}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(mmath_bin, nif_funcs, &load, NULL, &upgrade, NULL);
