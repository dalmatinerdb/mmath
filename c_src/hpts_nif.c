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
clean(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  ErlNifSInt64* target;
  unsigned count;
  unsigned j = 0;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (count % 2)
    return enif_make_badarg(env); // TODO return propper error

  if (!(target = (ErlNifSInt64*) enif_make_new_binary(env, (count / 2) * sizeof(ErlNifSInt64), &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (unsigned i = 0 ; i < count; i = i + 2) {
    target[j] = vs[i];
    j++;
  }
  return r;
}

char get_type(ErlNifSInt64 ev) {
  int64_t v = ntohll(ev);
  return (uint8_t)((v & TYPE_MASK) >> 56);
}

static ErlNifFunc nif_funcs[] = {
  /* {"from_list",    1, from_list}, */
  /* {"to_list",      1, to_list}, */
  {"clean",        1, clean}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(mmath_hpts, nif_funcs, &load, NULL, &upgrade, NULL);
