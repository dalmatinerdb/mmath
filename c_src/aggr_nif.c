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

  GET_BIN(0, bin, count, vs);

  if (!enif_get_int64(env, argv[1], &m))
    return enif_make_badarg(env);

  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, bin.size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (IS_SET(vs[i])) {
      target[i] = TO_DDB(dec_mul(FROM_DDB(vs[i]), m));
    } else {
      target[i] = 0;
    }
  }
  return r;
}

static ERL_NIF_TERM
mul_r(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
  ErlNifSInt64* vs;
  ErlNifSInt64 m;
  ErlNifSInt64* target;
  int count;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_BIN(0, bin, count, vs);

  if (!enif_get_int64(env, argv[1], &m))
    return enif_make_badarg(env);

  if (!m)
    return enif_make_badarg(env);

  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, bin.size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 0; i < count; i++) {
    if (IS_SET(vs[i])) {
      target[i] = TO_DDB(dec_div(FROM_DDB(vs[i]), m));
    } else {
      target[i] = 0;
    }
  }
  return r;
}

static ERL_NIF_TERM
divide_r(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
  decimal last;
  ErlNifSInt64* target;
  ErlNifSInt64* vs;
  int count;
  int has_value;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, bin, count, vs);

  if (count < 1) // can't be empty
    return enif_make_badarg(env);

  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, bin.size - sizeof(ErlNifSInt64), &r)))
    return enif_make_badarg(env); // TODO return propper error


  has_value = IS_SET(vs[0]);
  if (has_value) {
    last = FROM_DDB(vs[0]);
  }
  for (int i = 1; i < count; i++) {
    if (IS_SET(vs[i])) {
      if (has_value) {
        decimal this = FROM_DDB(vs[i]);
        target[i - 1] = TO_DDB(dec_sub(this, last));
        last = this;
      } else {
        target[i - 1] = 0;
        last = FROM_DDB(vs[i]);
        has_value = 1;
      }
    } else {
      if (has_value) {
        target[i - 1] = DDB_ZERO;
      } else {
        target[i - 1] = 0;
      }
    }
  }
  return r;
}

static ERL_NIF_TERM
derivate_r(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

  if (! (target = (decimal*) enif_make_new_binary(env, bin.size - sizeof(decimal), &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 1; i < count; i++) {
    target[i - 1] = dec_sub(vs[i], vs[i-1]);
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
  decimal aggr = {0, 0};      // aggregated value for this chunk
  int count;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_CHUNK(chunk);
  GET_BIN(0, bin, count, vs);

  target_size = ceil((double) count / chunk) * sizeof(ErlNifSInt64);
  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  if (count == 0)
    return r;
  pos = 0;
  for (int i = 0; i < count; i++, pos++) {
    if (pos == chunk) {
      target[target_i] = TO_DDB(aggr);
      target_i++;
      aggr.coefficient = !IS_SET(vs[i]);
      pos = 0;
    } else {
      aggr.coefficient += !IS_SET(vs[i]);
    }
  }

  if (count % chunk) {
    aggr.coefficient += (chunk - (count % chunk));
  }
  target[target_i] = TO_DDB(aggr);

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
  decimal aggr = {0, 0};      // aggregated value for this chunk
  int count;
  int has_value = 0;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_CHUNK(chunk);
  GET_BIN(0, bin, count, vs);

  target_size = ceil((double) count / chunk) * sizeof(ErlNifSInt64);
  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  // If we don't have any input data we can return right away.
  if (count == 0)
    return r;

  // We know we have at least one element in the list so our
  // aggregator will start with this
  if (IS_SET(vs[0])) {
    aggr = FROM_DDB(vs[0]);
    has_value = 1;
  };
  pos = 1;
  // We itterate over the remining i .. count-1 elements

  for (int i = 1; i < count; i++, pos++) {
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
      pos = 0;
    } else if (!has_value) {
      if (IS_SET(vs[i])) {
        aggr = FROM_DDB(vs[i]);
        has_value = 1;
      };
    } else {
      if (IS_SET(vs[i])) {
        decimal v = FROM_DDB(vs[i]);
        if (dec_cmp(v, aggr) < 0) {
          aggr = v;
        }
      }
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
min_r(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  decimal* vs;
  decimal* target;
  ErlNifSInt64 chunk;         // size to be compressed
  ErlNifSInt64 target_i = 0; // target position
  decimal aggr;         // target position
  uint32_t pos;
  uint32_t count;
  uint32_t target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_CHUNK(chunk);
  GET_BIN(0, bin, count, vs);

  target_size = ceil((double) count / chunk) * sizeof(decimal);
  if (! (target = (decimal*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  // If we don't have any input data we can return right away.
  if (count == 0)
    return r;

  // We know we have at least one element in the list so our
  // aggregator will start with this
  aggr = vs[0];
  pos = 1;
  // We itterate over the remining i .. count-1 elements
  for (uint32_t i = 1; i < count; i++, pos++) {
    if (pos == chunk) {
      target[target_i] = aggr;
      target_i++;
      aggr = vs[i];
      pos = 0;
    } else {
      if (dec_cmp(vs[i], aggr) < 0) {
        aggr = vs[i];
      };
    }
  }
  // Making sure the last aggregate is saved.
  if (target_i < target_size)
    target[target_i] = aggr;

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
  decimal aggr = {0, 0};      // aggregated value for this chunk
  int count;
  int has_value = 0;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_CHUNK(chunk);
  GET_BIN(0, bin, count, vs);

  target_size = ceil((double) count / chunk) * sizeof(ErlNifSInt64);
  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  // If we don't have any input data we can return right away.
  if (count == 0)
    return r;

  // We know we have at least one element in the list so our
  // aggregator will start with this
  if (IS_SET(vs[0])) {
    aggr = FROM_DDB(vs[0]);
    has_value = 1;
  };
  pos = 1;
  // We itterate over the remining i .. count-1 elements

  for (int i = 1; i < count; i++, pos++) {
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
      pos = 0;
    } else if (!has_value) {
      if (IS_SET(vs[i])) {
        aggr = FROM_DDB(vs[i]);
        has_value = 1;
      };
    } else {
      if (IS_SET(vs[i])) {
        decimal v = FROM_DDB(vs[i]);
        if (dec_cmp(v, aggr) > 0) {
          aggr = v;
        }
      }
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
max_r(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  decimal* vs;
  decimal* target;
  ErlNifSInt64 chunk;         // size to be compressed
  ErlNifSInt64 target_i = 0; // target position
  decimal aggr;         // target position
  uint32_t pos;
  uint32_t count;
  uint32_t target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_CHUNK(chunk);
  GET_BIN(0, bin, count, vs);

  target_size = ceil((double) count / chunk) * sizeof(decimal);
  if (! (target = (decimal*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  // If we don't have any input data we can return right away.
  if (count == 0)
    return r;

  // We know we have at least one element in the list so our
  // aggregator will start with this
  aggr = vs[0];
  pos = 1;
  // We itterate over the remining i .. count-1 elements
  for (uint32_t i = 1; i < count; i++, pos++) {
    if (pos == chunk) {
      target[target_i] = aggr;
      target_i++;
      aggr = vs[i];
      pos = 0;
    } else {
      if (dec_cmp(vs[i], aggr) > 0) {
        aggr = vs[i];
      };
    }
  }
  // Making sure the last aggregate is saved.
  target[target_i] = aggr;

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
  decimal aggr = {0, 0};      // aggregated value for this chunk
  decimal last = {0, 0};
  int count;
  int has_value = 0;
  int has_last = 0;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_CHUNK(chunk);
  GET_BIN(0, bin, count, vs);

  target_size = ceil((double) count / chunk) * sizeof(ErlNifSInt64);
  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  // If we don't have any input data we can return right away.
  if (count == 0)
    return r;

  // We know we have at least one element in the list so our
  // aggregator will start with this
  if (IS_SET(vs[0])) {
    aggr = FROM_DDB(vs[0]);
    last = aggr;
    has_value = 1;
    has_last = 1;
  };
  pos = 1;
  // We itterate over the remining i .. count-1 elements

  for (int i = 1; i < count; i++, pos++) {
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
      pos = 0;
    } else if (!has_value) {
      if (IS_SET(vs[i])) {
        aggr = FROM_DDB(vs[i]);
        last = aggr;
        has_value = 1;
        has_last = 1;
      };
    } else {
      if (IS_SET(vs[i])) {
        last = FROM_DDB(vs[i]);
        aggr = dec_add(aggr, last);
        has_last = 1;
        has_value = 1;
      } else if (has_last) {
        aggr = dec_add(aggr, last);
        has_value = 1;
      }
    }
  }
  if (has_value) {
    if (count % chunk) {
      aggr = dec_add(aggr, dec_mul(last, chunk - (count % chunk)));
    }
    target[target_i] = TO_DDB(aggr);
  } else {
    target[target_i] = 0;
  }
  return r;
}

static ERL_NIF_TERM
sum_r(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ErlNifSInt64 chunk;         // size to be compressed

  ERL_NIF_TERM r;
  decimal* vs;
  decimal* target;
  decimal aggr;          // Aggregator

  uint32_t target_i = 0;      // target position
  uint32_t count;
  uint32_t pos = 0;
  uint32_t target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_CHUNK(chunk);
  GET_BIN(0, bin, count, vs);

  target_size = ceil((double) count / chunk) * sizeof(decimal);
  if (! (target = (decimal*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error
  if (count > 0) {
    aggr = vs[0];
    pos = 1;
    for (uint32_t i = 1; i < count; i++, pos++) {
      if (pos == chunk) {
        target[target_i] = aggr;
        target_i++;
        aggr = vs[i];
        pos = 0;
      } else {
        aggr = dec_add(aggr, vs[i]);
      }
    }
    if (count % chunk) {
      target[target_i] = dec_add(aggr, dec_mul(vs[count - 1], (chunk - (count % chunk))));
    } else {
      target[target_i] = aggr;
    }
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
  decimal aggr = {0, 0};      // aggregated value for this chunk
  decimal last = {0, 0};
  int count;
  int has_value = 0;
  int has_last = 0;
  int target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_CHUNK(chunk);
  GET_BIN(0, bin, count, vs);

  target_size = ceil((double) count / chunk) * sizeof(ErlNifSInt64);
  if (! (target = (ErlNifSInt64*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  // If we don't have any input data we can return right away.
  if (count == 0)
    return r;

  // We know we have at least one element in the list so our
  // aggregator will start with this
  if (IS_SET(vs[0])) {
    aggr = FROM_DDB(vs[0]);
    last = aggr;
    has_value = 1;
    has_last = 1;
  };
  pos = 1;
  // We itterate over the remining i .. count-1 elements

  for (int i = 1; i < count; i++, pos++) {
    if (pos == chunk) {
      if (has_value) {
        target[target_i] = TO_DDB(dec_div(aggr, chunk));
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
      pos = 0;
    } else if (!has_value) {
      if (IS_SET(vs[i])) {
        aggr = FROM_DDB(vs[i]);
        last = aggr;
        has_value = 1;
        has_last = 1;
      };
    } else {
      if (IS_SET(vs[i])) {
        last = FROM_DDB(vs[i]);
        aggr = dec_add(aggr, last);
        has_last = 1;
        has_value = 1;
      } else if (has_last) {
        aggr = dec_add(aggr, last);
        has_value = 1;
      }
    }
  }

  if (has_value) {
    if (count % chunk) {
      aggr = dec_add(aggr, dec_mul(last, (chunk - (count % chunk))));
    }
    target[target_i] = TO_DDB(dec_div(aggr, chunk));
  } else {
    target[target_i] = 0;
  }
  return r;
}

static ERL_NIF_TERM
avg_r(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  decimal* vs;
  decimal* target;
  ErlNifSInt64 chunk;         // size to be compressed
  decimal aggr;               // Aggregator

  uint32_t target_i = 0;      // target position
  uint32_t count;
  uint32_t pos = 0;
  uint32_t target_size;

  if (argc != 2)
    return enif_make_badarg(env);

  GET_CHUNK(chunk);
  GET_BIN(0, bin, count, vs);

  target_size = ceil((double) count / chunk) * sizeof(decimal);
  if (! (target = (decimal*) enif_make_new_binary(env, target_size, &r)))
    return enif_make_badarg(env); // TODO return propper error

  if (count == 0)
    return r;

  aggr = vs[0];
  pos++;

  for (uint32_t i = 1; i < count; i++, pos++) {
    if (pos == chunk) {
      target[target_i] =  dec_div(aggr, chunk);
      target_i++;
      aggr = vs[i];
      pos = 0;
    } else {
      aggr = dec_add(aggr, vs[i]);
    }
  }
  if (count % chunk) {
    aggr = dec_add(aggr, dec_mul(vs[count - 1], (chunk - (count % chunk))));
  }
  target[target_i] = dec_div(aggr, chunk);

  return r;
}

static ErlNifFunc nif_funcs[] = {
  {"mul",        2, mul},
  {"mul_r",      2, mul_r},
  {"divide",     2, divide},
  {"divide_r",   2, divide_r},
  {"empty",      2, empty},
  {"min",        2, min},
  {"min_r",      2, min_r},
  {"max",        2, max},
  {"max_r",      2, max_r},
  {"sum",        2, sum},
  {"sum_r",      2, sum_r},
  {"avg",        2, avg},
  {"avg_r",      2, avg_r},
  {"derivate",   1, derivate},
  {"derivate_r", 1, derivate_r}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(mmath_aggr, nif_funcs, &load, NULL, &upgrade, NULL);
