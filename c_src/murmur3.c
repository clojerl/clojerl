//-----------------------------------------------------------------------------
// MurmurHash3 was written by Austin Appleby, and is placed in the public
// domain. The author hereby disclaims copyright to this source code.
//-----------------------------------------------------------------------------

#include "murmur3.h"

#ifdef __GNUC__
#define FORCE_INLINE __attribute__((always_inline)) inline
#else
#define FORCE_INLINE inline
#endif

static FORCE_INLINE uint32_t rotl32 ( uint32_t x, int8_t r )
{
  return (x << r) | (x >> (32 - r));
}

#define	ROTL(x,y)	rotl32(x,y)

#define SEED            0
#define C1              0xcc9e2d51
#define C2              0x1b873593

// Finalization mix - force all bits of a hash block to avalanche
static FORCE_INLINE uint32_t fmix (uint32_t h, uint32_t len)
{
  h ^= len;
  h ^= h >> 16;
  h *= 0x85ebca6b;
  h ^= h >> 13;
  h *= 0xc2b2ae35;
  h ^= h >> 16;

  return h;
}

//-----------------------------------------------------------------------------

uint32_t murmur_mix_coll_hash(uint32_t hash, uint32_t len);
uint32_t mixK1(uint32_t k1);
uint32_t mixH1(uint32_t h1, uint32_t k1);

uint32_t murmur_ordered_coll(uint32_t * hashes, uint32_t len)
{
  uint32_t hash = 1;
  for(uint32_t i = 0; i < len; i = i + 1) {
    hash = 31 * hash + hashes[i];
  }
  return murmur_mix_coll_hash(hash, len);
}

uint32_t murmur_unordered_coll(uint32_t * hashes, uint32_t len)
{
  uint32_t hash = 1;
  for(uint32_t i = 0; i < len; i = i + 1) {
    hash = hash + hashes[i];
  }
  return murmur_mix_coll_hash(hash, len);
}

//-----------------------------------------------------------------------------
// Helper functions
//-----------------------------------------------------------------------------

uint32_t murmur_mix_coll_hash(uint32_t hash, uint32_t len){
  uint32_t h1 = SEED;
  uint32_t k1 = mixK1(hash);
  h1 = mixH1(h1, k1);
  return fmix(h1, len);
}

uint32_t mixK1(uint32_t k1) {
  k1 *= C1;
  k1 = ROTL(k1, 15);
  k1 *= C2;
  return k1;
}

uint32_t mixH1(uint32_t h1, uint32_t k1) {
  h1 ^= k1;
  h1 = ROTL(h1, 13);
  h1 = h1 * 5 + 0xe6546b64;
  return h1;
}
