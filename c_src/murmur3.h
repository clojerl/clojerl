// -----------------------------------------------------------------------------
// MurmurHash3 was written by Austin Appleby, and is placed in the
// public domain. The author hereby disclaims copyright to this source
// code.
// -----------------------------------------------------------------------------

#ifndef _murmur3_h_
#define _murmur3_h_

#include <stdint.h>

uint32_t murmur_ordered_coll(uint32_t* hashes, uint32_t len);

uint32_t murmur_unordered_coll(uint32_t* hashes, uint32_t len);

//-----------------------------------------------------------------------------

#endif // _murmur3_h_
