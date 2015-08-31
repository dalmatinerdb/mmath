#include "erl_nif.h"

// There are four functions that may be called during the lifetime
// of a NIF. load, reload, upgrade, and unload. Any of these functions
// can be left unspecified by passing NULL to the ERL_NIF_INIT macro.
//
// NIFs are awesome.

// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#load

#define IS_SET(v) ((v & 0x00000000000000FFLL) != 0)
#define FROM_DDB(v) ((ErlNifSInt64) htonll((v & 0x0000000000000100LL) ? ((v & 0xFFFFFFFFFFFFFF00LL) | 0x00000000000000FFLL) : (v & 0xFFFFFFFFFFFFFF00LL)))
#define TO_DDB(v) ((ntohll(v) & 0xFFFFFFFFFFFFFF00LL) | 0x0000000000000001LL)

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

// Called when changing versions of the C code for a module's NIF
// implementation if I read the docs correctly.
//
// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#upgrade

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
  return 0;
}

// Called when the library is unloaded. Not called after a reload
// executes.
//
// No return value
// Docs: http://erlang.org/doc/man/erl_nif.html#load

static void
unload(ErlNifEnv* env, void* priv)
{
  return;
}

// The actual C implementation of an Erlang function.
//
// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc

static ERL_NIF_TERM
mul(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  ErlNifSInt64 m;
  ErlNifSInt64* target;
  int count;

  if (argc != 2)
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[0], &bin)) // needs to be a multiple of 8
    return enif_make_badarg(env);

  if (((bin.size % sizeof(ErlNifUInt64)) != 0)) // needs to be a multiple of 8
    return enif_make_badarg(env);
  count = bin.size / sizeof(ErlNifUInt64);
  vs = (ErlNifSInt64 *) bin.data;

  if (!enif_get_int64(env, argv[1], &m))
    return enif_make_badarg(env);

  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, bin.size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (IS_SET(vs[i])) {
      target[i] = TO_DDB(FROM_DDB(vs[i]) * m);
    } else {
      target[i] = vs[i];
    }
  }
  return r;
}

static ERL_NIF_TERM
divide(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  ErlNifSInt64 m;
  ErlNifSInt64* target;
  int count;

  if (argc != 2)
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[0], &bin)) // needs to be a multiple of 8
    return enif_make_badarg(env);

  if (((bin.size % sizeof(ErlNifUInt64)) != 0)) // needs to be a multiple of 8
    return enif_make_badarg(env);
  count = bin.size / sizeof(ErlNifUInt64);
  vs = (ErlNifSInt64 *) bin.data;

  if (!enif_get_int64(env, argv[1], &m))
    return enif_make_badarg(env);

  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, bin.size, &r)))
    return enif_make_badarg(env); // TODO return propper error


  for (int i = 0; i < count; i++) {
    if (IS_SET(vs[i])) {
      //printf("%ld / %ld = %ld\r\n", FROM_DDB(vs[i]), m, FROM_DDB(vs[i]) / m);
      //printf("%016lx\r\n", vs[i]);
      //printf("%016llx\r\n", TO_DDB(FROM_DDB(vs[i]) / m));
      target[i] = TO_DDB(FROM_DDB(vs[i]) / m);
    } else {
      target[i] = vs[i];
    }
  }
  return r;
}

static ERL_NIF_TERM
derivate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  ErlNifSInt64 last;
  ErlNifSInt64* target;
  ErlNifSInt64* vs;
  int count;
  int has_value;

  if (argc != 1)
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[0], &bin)) // needs to be a multiple of 8
    return enif_make_badarg(env);

  if (((bin.size % sizeof(ErlNifUInt64)) != 0)) // needs to be a multiple of 8
    return enif_make_badarg(env);

  count = bin.size / sizeof(ErlNifUInt64);
  if (count < 1) // can't be empty
    return enif_make_badarg(env);
  vs = (ErlNifSInt64 *) bin.data;

  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, bin.size - sizeof(ErlNifUInt64), &r)))
    return enif_make_badarg(env); // TODO return propper error

  last = FROM_DDB(vs[0]);
  has_value = IS_SET(vs[0]);
  for (int i = 1; i < count; i++) {
    if (IS_SET(vs[i])) {
      if (has_value) {
        ErlNifSInt64 this = FROM_DDB(vs[i]);
        target[i - 1] = TO_DDB(this - last);
        last = this;
      } else {
        target[i - 1] = 0;
        last = FROM_DDB(vs[i]);
        has_value = 1;
      }
    } else {
      if (has_value) {
        target[i - 1] = TO_DDB(0);
      } else {
        target[i - 1] = 0;
      }
    }
  }
  return r;
}

static ERL_NIF_TERM
min(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  ErlNifSInt64* target;
  ErlNifSInt64 chunk;         // size to be compressed
  ErlNifSInt64 target_i = 0;  // target position
  ErlNifSInt64 pos = 0;       // position in chunk
  ErlNifSInt64 aggr = 0;      // aggregated value for this chunk
  int count;
  int has_value = 0;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[0], &bin))
    return enif_make_badarg(env);

  if (((bin.size % sizeof(ErlNifUInt64)) != 0)) // needs to be a multiple of 8
    return enif_make_badarg(env);

  count = bin.size / sizeof(ErlNifUInt64);
  vs = (ErlNifSInt64 *) bin.data;

  if (!enif_get_int64(env, argv[1], &chunk))
    return enif_make_badarg(env);

  target_size = ceil((double) count / chunk) * sizeof(ErlNifUInt64);
  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (pos == chunk) {
      if (has_value) {
        target[target_i] = TO_DDB(aggr);
      } else {
        target[target_i] = 0;
      }
      target_i++;
      has_value = IS_SET(vs[i]);
      if (has_value) {
        aggr = FROM_DDB(vs[i]);
      };
      pos = 1;
    } else if (!has_value) {
      if (IS_SET(vs[i])) {
        aggr = FROM_DDB(vs[i]);
        has_value = 1;
      };
      pos++;
    } else  {
      if (IS_SET(vs[i])) {
        ErlNifSInt64 v = FROM_DDB(vs[i]);
        if (v < aggr) {
          aggr = v;
        }
      }
      pos++;
    }
  }
  if (has_value) {
    target[target_i] = TO_DDB(aggr);
  } else {
    target[target_i] = 0;
  }
  return r;
}

static ERL_NIF_TERM
max(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  ErlNifSInt64* target;
  ErlNifSInt64 chunk;         // size to be compressed
  ErlNifSInt64 target_i = 0;  // target position
  ErlNifSInt64 pos = 0;       // position in chunk
  ErlNifSInt64 aggr = 0;      // aggregated value for this chunk
  int count;
  int has_value = 0;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[0], &bin))
    return enif_make_badarg(env);

  if (((bin.size % sizeof(ErlNifUInt64)) != 0)) // needs to be a multiple of 8
    return enif_make_badarg(env);

  count = bin.size / sizeof(ErlNifUInt64);
  vs = (ErlNifSInt64 *) bin.data;

  if (!enif_get_int64(env, argv[1], &chunk))
    return enif_make_badarg(env);

  target_size = ceil((double) count / chunk) * sizeof(ErlNifUInt64);
  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (pos == chunk) {
      if (has_value) {
        target[target_i] = TO_DDB(aggr);
      } else {
        target[target_i] = 0;
      }
      target_i++;
      has_value = IS_SET(vs[i]);
      if (has_value) {
        aggr = FROM_DDB(vs[i]);
      };
      pos = 1;
    } else if (!has_value) {
      if (IS_SET(vs[i])) {
        aggr = FROM_DDB(vs[i]);
        has_value = 1;
      };
      pos++;
    } else  {
      if (IS_SET(vs[i])) {
        ErlNifSInt64 v = FROM_DDB(vs[i]);
        if (v > aggr) {
          aggr = v;
        }
      }
      pos++;
    }
  }
  if (has_value) {
    target[target_i] = TO_DDB(aggr);
  } else {
    target[target_i] = 0;
  }
  return r;
}

static ERL_NIF_TERM
sum(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  ErlNifSInt64* target;
  ErlNifSInt64 chunk;         // size to be compressed
  ErlNifSInt64 target_i = 0;  // target position
  ErlNifSInt64 pos = 0;       // position in chunk
  ErlNifSInt64 aggr = 0;      // aggregated value for this chunk
  ErlNifSInt64 last = 0;
  int count;
  int has_value = 0;
  int has_last = 0;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[0], &bin))
    return enif_make_badarg(env);

  if (((bin.size % sizeof(ErlNifUInt64)) != 0)) // needs to be a multiple of 8
    return enif_make_badarg(env);

  count = bin.size / sizeof(ErlNifUInt64);
  vs = (ErlNifSInt64 *) bin.data;

  if (!enif_get_int64(env, argv[1], &chunk))
    return enif_make_badarg(env);

  target_size = ceil((double) count / chunk) * sizeof(ErlNifUInt64);
  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (pos == chunk) {
      if (has_value) {
        target[target_i] = TO_DDB(aggr);
      } else {
        target[target_i] = 0;
      }
      target_i++;
      has_value = IS_SET(vs[i]);
      if (has_value) {
        aggr = FROM_DDB(vs[i]);
        last = aggr;
        has_last = 1;
      } else if (has_last) {
        aggr = last;
        has_value = 1;
      }
      pos = 1;
    } else if (!has_value) {
      if (IS_SET(vs[i])) {
        aggr = FROM_DDB(vs[i]);
        last = aggr;
        has_value = 1;
        has_last = 1;
      };
      pos++;
    } else {
      if (IS_SET(vs[i])) {
        last = FROM_DDB(vs[i]);
        aggr += last;
        has_last = 1;
        has_value = 1;
      } else if (has_last) {
        aggr += last;
        has_value = 1;
      }
      pos++;
    }
  }
  if (has_value) {
    target[target_i] = TO_DDB(aggr + (last * (chunk - pos)));
  } else {
    target[target_i] = 0;
  }
  return r;
}

static ERL_NIF_TERM
avg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  ErlNifSInt64* vs;
  ErlNifSInt64* target;
  ErlNifSInt64 chunk;         // size to be compressed
  ErlNifSInt64 target_i = 0;  // target position
  ErlNifSInt64 pos = 0;       // position in chunk
  ErlNifSInt64 aggr = 0;      // aggregated value for this chunk
  ErlNifSInt64 last = 0;
  int count;
  int has_value = 0;
  int has_last = 0;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[0], &bin))
    return enif_make_badarg(env);

  if (((bin.size % sizeof(ErlNifUInt64)) != 0)) // needs to be a multiple of 8
    return enif_make_badarg(env);

  count = bin.size / sizeof(ErlNifUInt64);
  vs = (ErlNifSInt64 *) bin.data;

  if (!enif_get_int64(env, argv[1], &chunk))
    return enif_make_badarg(env);

  target_size = ceil((double) count / chunk) * sizeof(ErlNifUInt64);
  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (pos == chunk) {
      if (has_value) {
        target[target_i] = TO_DDB(aggr / chunk);
      } else {
        target[target_i] = 0;
      }
      target_i++;
      has_value = IS_SET(vs[i]);
      if (has_value) {
        aggr = FROM_DDB(vs[i]);
        last = aggr;
        has_last = 1;
      } else if (has_last) {
        aggr = last;
        has_value = 1;
      }
      pos = 1;
    } else if (!has_value) {
      if (IS_SET(vs[i])) {
        aggr = FROM_DDB(vs[i]);
        last = aggr;
        has_value = 1;
        has_last = 1;
      };
      pos++;
    } else {
      if (IS_SET(vs[i])) {
        last = FROM_DDB(vs[i]);
        aggr += last;
        has_last = 1;
        has_value = 1;
      } else if (has_last) {
        aggr += last;
        has_value = 1;
      }
      pos++;
    }
  }
  if (has_value) {
    target[target_i] = TO_DDB((aggr + (last * (chunk - pos))) / chunk);
  } else {
    target[target_i] = 0;
  }
  return r;
}



static ErlNifFunc nif_funcs[] = {
  {"mul",      2, mul},
  {"divide",   2, divide},
  {"max",      2, max},
  {"min",      2, min},
  {"sum",      2, sum},
  {"avg",      2, avg},
  {"derivate", 1, derivate}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(mmath_aggr, nif_funcs, &load, NULL, &upgrade, &unload);

// Or if you don't need reload, upgrade, or unload.
// ERL_NIF_INIT(skeleton, nif_funcs, &load, NULL, NULL, NULL);
