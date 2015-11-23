#if defined(__linux__)
#  define __USE_BSD
#  include <stdint.h>
#  include <endian.h>
#  define htonll(v) htobe64(v)
#  define ntohll(v) be64toh(v)
#elif defined(__APPLE__)
#  include <libkern/OSByteOrder.h>
#  define htonll(v) OSSwapHostToBigInt64(v)
#  define ntohll(v) OSSwapBigToHostInt64(v)
#elif defined(SOLARIS)
#  include <sys/byteorder.h>
#endif

typedef struct {
    long long coefficient;
    char exponent;
} decimal;

#define DDB_ZERO htonll(0x0100000000000000LL)
#define IS_SET(v) ((v & 0x00000000000000FFLL) != 0)
#define FROM_DDB(v) dec_deserialize(v)
#define TO_DDB(v) dec_serialize(v)

#define GET_CHUNK(chunk)                                                \
  if (!enif_get_int64(env, argv[1], &chunk))                            \
    return enif_make_badarg(env);                                       \
  if (chunk < 1)                                                        \
    return enif_make_badarg(env)

#define GET_BIN(pos, bin, count, vs)              \
  if (!enif_inspect_binary(env, argv[pos], &bin)) \
    return enif_make_badarg(env);                 \
  if (bin.size % sizeof(__typeof__(*vs)))              \
    return enif_make_badarg(env);                 \
  count = bin.size / sizeof(__typeof__(*vs));          \
  vs = (__typeof__(vs)) bin.data


ErlNifSInt64 dec_serialize(decimal v);
decimal dec_deserialize(ErlNifSInt64 v);

decimal dec_from_int64(ErlNifSInt64 v);
decimal dec_from_double(double v);

decimal dec_mul(decimal v, long long m);
decimal dec_div(decimal v, long long m);
decimal dec_add(decimal a, decimal b);
decimal dec_add3(decimal a, decimal b, decimal c);
decimal dec_sub(decimal a, decimal b);
int dec_cmp(decimal a, decimal b);
