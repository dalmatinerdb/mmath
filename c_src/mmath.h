#if defined(__linux__)
#  define __USE_BSD
#  include <stdint.h>
#  include <endian.h>
#  define htonll(v) htobe64(v)
#  define ntohll(v) be64toh(v)
/* On modern Os X'es (>= 10.10) htonll is defined in machine/endian.h, but on older it is missing */
#elif defined(__APPLE__) && !defined(htonll)
#  include <libkern/OSByteOrder.h>
#  define htonll(v) OSSwapHostToBigInt64(v)
#  define ntohll(v) OSSwapBigToHostInt64(v)
#elif defined(SOLARIS)
#  include <sys/byteorder.h>
#endif

#define CERTAIN 100000

typedef struct {
  double value;
  uint32_t confidence;
} ffloat;

#define DDB_ZERO htonll(0x0100000000000000LL)
#define DDB_UNSET htonll(0x0000000000000000LL)
#define IS_SET(v) ((ntohll(v) & 0xFF00000000000000LL) != 0LL)
#define FROM_DDB(v) float_deserialize(v)
#define TO_DDB(v) float_serialize(v)

#define GET_CHUNK(chunk)                                                \
  if (!enif_get_int64(env, argv[1], &chunk))                            \
    return enif_make_badarg(env);                                       \
  if (chunk < 1)                                                        \
    return enif_make_badarg(env);

#define GET_BIN(pos, bin, count, vs)              \
  if (!enif_inspect_binary(env, argv[pos], &bin)) \
    return enif_make_badarg(env);                 \
  if (bin.size % sizeof(__typeof__(*vs)))         \
    return enif_make_badarg(env);                 \
  count = bin.size / sizeof(__typeof__(*vs));     \
  vs = (__typeof__(vs)) bin.data;


ErlNifSInt64 float_serialize(ffloat f);
ffloat float_deserialize(ErlNifSInt64 v);

ffloat float_from_int64(int64_t v);
ffloat float_from_double(double v);
ffloat float_from_binary(int len, char* str);

ffloat float_mul(ffloat v, double m);
ffloat float_div(ffloat v, double m);
ffloat float_add(ffloat a, ffloat b);
ffloat float_add3(ffloat a, ffloat b, ffloat c);
ffloat float_sub(ffloat a, ffloat b);
