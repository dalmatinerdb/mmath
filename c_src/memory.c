// original source: https://gist.github.com/ahf/589a4a04145efd9ba43ed9fbce63ee8d

#include <arpa/inet.h>

uint16_t celo_hton16(uint16_t value)
{
    return htons(value);
}

uint16_t celo_ntoh16(uint16_t value)
{
    return ntohs(value);
}

uint32_t celo_hton32(uint32_t value)
{
    return htonl(value);
}

uint32_t celo_ntoh32(uint32_t value)
{
    return ntohl(value);
}

uint64_t celo_hton64(uint64_t value)
{
    if (1 == celo_hton16(1))
        return value;

    return ((uint64_t)celo_hton32(value & 0xFFFFFFFF) << 32) | celo_hton32(value >> 32);
}

uint64_t celo_ntoh64(uint64_t value)
{
    if (1 == celo_hton16(1))
        return value;

    return ((uint64_t)celo_ntoh32(value & 0xFFFFFFFF) << 32) | celo_ntoh32(value >> 32);
}
