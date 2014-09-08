#include "ed25519-donna-portable-identify.h"

#define mul32x32_64(a,b) (((uint64_t)(a))*(b))

/* platform */
#if defined(COMPILER_MSVC)
	#include <intrin.h>
	#if !defined(_DEBUG)
		#undef mul32x32_64		
		#define mul32x32_64(a,b) __emulu(a,b)
	#endif
	#undef inline
	#define inline __forceinline
	#define MM16 __declspec(align(16))
	#define ROTL32(a,b) _rotl(a,b)
	#define ROTR32(a,b) _rotr(a,b)
#else
	#include <sys/param.h>
	#undef inline
	#define inline __attribute__((always_inline))
	#define MM16 __attribute__((aligned(16)))
	#define ROTL32(a,b) (((a) << (b)) | ((a) >> (32 - b)))
	#define ROTR32(a,b) (((a) >> (b)) | ((a) << (32 - b)))
#endif

/* uint128_t */
#if defined(CPU_64BITS)
	#if defined(COMPILER_MSVC)
		#define HAVE_UINT128
		struct uint128 {
			uint64_t lo, hi;
		};
		typedef struct uint128 uint128_t;
		#define mul64x64_128(out,a,b) out.lo = _umul128(a,b,&out.hi);
		#define shr128_pair(out,hi,lo,shift) out = __shiftright128(lo, hi, shift);
		#define shr128(out,in,shift) shr128_pair(out, in.hi, in.lo, shift)
		#define add128(a,b) { uint64_t p = a.lo; a.lo += b.lo; a.hi += b.hi + (a.lo < p); }
		#define add128_64(a,b) { uint64_t p = a.lo; a.lo += b; a.hi += (a.lo < p); }
		#define lo128(a) (a.lo)
	#elif (defined(CPU_X86_64) && defined(COMPILER_GCC)) && (defined(COMPILER_INTEL) || defined(COMPILER_PATHCC))
		#define HAVE_UINT128
		struct uint128 {
			uint64_t lo, hi;
		};
		typedef struct uint128 uint128_t;
		#define mul64x64_128(out,a,b) __asm__ ("mulq %3" : "=a" (out.lo), "=d" (out.hi) : "a" (a), "rm" (b));
		#define shr128_pair(out,hi,lo,shift) __asm__ ("shrdq %3,%2,%0" : "=r" (lo) : "0" (lo), "r" (hi), "J" (shift)); out = lo;
		#define shr128(out,in,shift) shr128_pair(out,in.hi, in.lo, shift)
		#define add128(a,b) __asm__ ("addq %4,%2; adcq %5,%3" : "=r" (a.hi), "=r" (a.lo) : "1" (a.lo), "0" (a.hi), "rm" (b.lo), "rm" (b.hi) : "cc");
		#define add128_64(a,b) __asm__ ("addq %4,%2; adcq $0,%3" : "=r" (a.hi), "=r" (a.lo) : "1" (a.lo), "0" (a.hi), "rm" (b) : "cc");
		#define lo128(a) (a.lo)

	#elif defined(COMPILER_GCC)
		#define HAVE_NATIVE_UINT128
		#define HAVE_UINT128
		typedef unsigned uint128_t __attribute__((mode(TI)));

		#define mul64x64_128(out,a,b) out = (uint128_t)a * b;
		#define shr128_pair(out,hi,lo,shift) out = (uint64_t)((((uint128_t)hi << 64) | lo) >> shift);
		#define shr128(out,in,shift) out = (uint64_t)(in >> shift);
		#define add128(a,b) a += b;
		#define add128_64(a,b) a += b;
		#define lo128(a) ((uint64_t)a)
	#else
		/* need 128bit define for this compiler */
	#endif
#endif

/* endian */
static inline void U32TO8_LE(unsigned char *p, const uint32_t v) {
	p[0] = (unsigned char)(v      );
	p[1] = (unsigned char)(v >>  8);
	p[2] = (unsigned char)(v >> 16);
	p[3] = (unsigned char)(v >> 24);
}

#if !defined(HAVE_UINT128)
static inline uint32_t U8TO32_LE(const unsigned char *p) {
	return
	(((uint32_t)(p[0])      ) | 
	 ((uint32_t)(p[1]) <<  8) |
	 ((uint32_t)(p[2]) << 16) |
	 ((uint32_t)(p[3]) << 24));
}
#else
static inline uint64_t U8TO64_LE(const unsigned char *p) {
	return
	(((uint64_t)(p[0])      ) |
	 ((uint64_t)(p[1]) <<  8) |
	 ((uint64_t)(p[2]) << 16) |
	 ((uint64_t)(p[3]) << 24) |
	 ((uint64_t)(p[4]) << 32) |
	 ((uint64_t)(p[5]) << 40) |
	 ((uint64_t)(p[6]) << 48) |
	 ((uint64_t)(p[7]) << 56));
}

static inline void U64TO8_LE(unsigned char *p, const uint64_t v) {
	p[0] = (unsigned char)(v      );
	p[1] = (unsigned char)(v >>  8);
	p[2] = (unsigned char)(v >> 16);
	p[3] = (unsigned char)(v >> 24);
	p[4] = (unsigned char)(v >> 32);
	p[5] = (unsigned char)(v >> 40);
	p[6] = (unsigned char)(v >> 48);
	p[7] = (unsigned char)(v >> 56);
}
#endif

#define DONNA_INLINE inline

#include <stdlib.h>
#include <string.h>


