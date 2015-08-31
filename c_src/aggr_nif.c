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
  ErlNifSInt64* vs;
  ErlNifSInt64 m;
  ErlNifSInt64* target;
  int count;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_BIN(1, bin, count, vs);

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

  GET_BIN(1, bin, count, vs);

  if (!enif_get_int64(env, argv[1], &m))
    return enif_make_badarg(env);

  if (!m)
    return enif_make_badarg(env);

  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, bin.size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (IS_SET(vs[i])) {
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

  GET_BIN(1, bin, count, vs);

  if (count < 1) // can't be empty
    return enif_make_badarg(env);

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
empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_CHUNK(chunk);
  GET_BIN(1, bin, count, vs);

  count = bin.size / sizeof(ErlNifUInt64);
  vs = (ErlNifSInt64 *) bin.data;

  target_size = ceil((double) count / chunk) * sizeof(ErlNifUInt64);
  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (pos == chunk) {
      target[target_i] = TO_DDB(aggr);
      target_i++;
      aggr = !IS_SET(vs[i]);
      pos = 1;
    } else {
      aggr += !IS_SET(vs[i]);
      pos++;
    }
  }
  target[target_i] = TO_DDB(aggr + (chunk - pos));
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

  GET_CHUNK(chunk);
  GET_BIN(1, bin, count, vs);

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

  GET_CHUNK(chunk);
  GET_BIN(1, bin, count, vs);

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

  GET_CHUNK(chunk);
  GET_BIN(1, bin, count, vs);

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

  GET_CHUNK(chunk);
  GET_BIN(1, bin, count, vs);

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
  {"empty",    2, empty},
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

ERL_NIF_INIT(mmath_aggr, nif_funcs, &load, NULL, &upgrade, NULL);
