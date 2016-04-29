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

  if (! (target = (decimal*) enif_make_new_binary(env, bin.size - sizeof(decimal), &r)))
    return enif_make_badarg(env); // TODO return propper error

  for (int i = 1; i < count; i++) {
    target[i - 1] = dec_sub(vs[i], vs[i-1]);
  }
  return r;
}

static ERL_NIF_TERM
min(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  decimal* vs;
  decimal* target;
  ErlNifSInt64 chunk;         // size to be compressed
  ErlNifSInt64 target_i = 0; // target position
  decimal aggr;         // target position
  uint64_t confidence;
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
  confidence = aggr.confidence;
  pos = 1;
  // We itterate over the remining i .. count-1 elements
  for (uint32_t i = 1; i < count; i++, pos++) {
    if (pos == chunk) {
      aggr.confidence = confidence / chunk;
      target[target_i] = aggr;
      target_i++;
      aggr = vs[i];
      confidence = aggr.confidence;
      pos = 0;
    } else {
      confidence += vs[i].confidence;
      if (dec_cmp(vs[i], aggr) < 0) {
        aggr = vs[i];
      };
    }
  }
  // Making sure the last aggregate is saved.
  if (target_i < target_size) {
    // We use chunk here to reflect the additional loss
    // in certenty of not computing a whole chunk.
    aggr.confidence = confidence / chunk;
    target[target_i] = aggr;
  }

  return r;
}

static ERL_NIF_TERM
max(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  decimal* vs;
  decimal* target;
  ErlNifSInt64 chunk;         // size to be compressed
  ErlNifSInt64 target_i = 0; // target position
  decimal aggr;         // target position
  uint64_t confidence = 0;
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
  confidence = aggr.confidence;
  pos = 1;
  // We itterate over the remining i .. count-1 elements
  for (uint32_t i = 1; i < count; i++, pos++) {
    if (pos == chunk) {
      aggr.confidence = confidence / chunk;
      target[target_i] = aggr;
      target_i++;
      aggr = vs[i];
      confidence = aggr.confidence;
      pos = 0;
    } else {
      confidence += vs[i].confidence;
      if (dec_cmp(vs[i], aggr) > 0) {
        aggr = vs[i];
      };
    }
  }
  // Making sure the last aggregate is saved.
  if (target_i < target_size) {
    // We use chunk here to reflect the additional loss
    // in certenty of not computing a whole chunk.
    aggr.confidence = confidence / chunk;
    target[target_i] = aggr;
  }
  return r;
}

static ERL_NIF_TERM
sum(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ErlNifSInt64 chunk;         // size to be compressed

  ERL_NIF_TERM r;
  decimal* vs;
  decimal* target;
  decimal aggr;          // Aggregator
  uint64_t confidence;

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
    confidence = aggr.confidence;
    pos = 1;
    //We will be overwriting the confidence generated by dec_add because
    //it would give a false impression based on the later values having
    //a higher influence.
    for (uint32_t i = 1; i < count; i++, pos++) {
      if (pos == chunk) {
        aggr.confidence = confidence / chunk;
        target[target_i] = aggr;
        target_i++;
        aggr = vs[i];
        confidence = aggr.confidence;

        pos = 0;
      } else {
        confidence += vs[i].confidence;
        aggr = dec_add(aggr, vs[i]);
      }
    }
    if (count % chunk) {
      aggr = dec_add(aggr, dec_mul(vs[count - 1], (chunk - (count % chunk))));
    }
    aggr.confidence = confidence / chunk;
    target[target_i] = aggr;
  }
  return r;
}

static ERL_NIF_TERM
avg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM r;
  decimal* vs;
  decimal* target;
  ErlNifSInt64 chunk;         // size to be compressed
  decimal aggr;               // Aggregator
  uint64_t confidence;
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
  confidence = aggr.confidence;
  pos++;

  for (uint32_t i = 1; i < count; i++, pos++) {
    if (pos == chunk) {
      aggr.confidence = confidence / chunk;
      target[target_i] =  dec_div(aggr, chunk);
      target_i++;
      aggr = vs[i];
      confidence = aggr.confidence;
      pos = 0;
    } else {
      confidence += vs[i].confidence;
      aggr = dec_add(aggr, vs[i]);
    }
  }
  if (count % chunk) {
    aggr = dec_add(aggr, dec_mul(vs[count - 1], (chunk - (count % chunk))));
  }
  aggr.confidence = confidence / chunk;
  target[target_i] = dec_div(aggr, chunk);

  return r;
}

static ERL_NIF_TERM
confidence(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary a;
  ERL_NIF_TERM r;
  decimal temp = {.exponent = -3, .confidence = CERTAIN};
  decimal* vs;
  decimal* target;
  int count;

  if (argc != 1)
    return enif_make_badarg(env);

  GET_BIN(0, a, count, vs);

  if (! (target = (decimal*) enif_make_new_binary(env, count * sizeof(decimal), &r)))
    return enif_make_badarg(env); // TODO return propper error
  for (int i = 0; i < count; i++) {
    target[i] = temp;
    target[i].coefficient = vs[i].confidence;
  }
  return r;
}

static ErlNifFunc nif_funcs[] = {
  {"min",        2, min},
  {"max",        2, max},
  {"sum",        2, sum},
  {"avg",        2, avg},
  {"mul",        2, mul},
  {"divide",     2, divide},
  {"derivate",   1, derivate},
  {"confidence", 1, confidence}

};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(mmath_aggr, nif_funcs, &load, NULL, &upgrade, NULL);
