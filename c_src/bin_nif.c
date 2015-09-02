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
realize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  ErlNifSInt64* target;
  ErlNifSInt64 last = 0;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, count * sizeof(ErlNifSInt64), &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (IS_SET(vs[i])) {
      last = FROM_DDB(vs[i]);
    };
    target[i] = last;
  }
  return r;
}

derealize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
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
  {"realize",      1, realize},
  {"derealize",    1, derealize}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(mmath_bin, nif_funcs, &load, NULL, &upgrade, NULL);
