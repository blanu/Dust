/*
	Public domain by Adam Langley <agl@imperialviolet.org> &
	                 Andrew M. <liquidsun@gmail.com>
	See: https://github.com/floodyberry/curve25519-donna

	64bit integer curve25519 implementation
*/

typedef uint64_t bignum25519[5];

static const uint64_t reduce_mask_40 = 0x000000ffffffffffull;
static const uint64_t reduce_mask_51 = 0x0007ffffffffffffull;
static const uint64_t reduce_mask_56 = 0x00ffffffffffffffull;

/* out = in */
static void DONNA_INLINE
curve25519_copy(bignum25519 out, const bignum25519 in) {
	out[0] = in[0];
	out[1] = in[1];
	out[2] = in[2];
	out[3] = in[3];
	out[4] = in[4];
}

/* out = a + b */
static void DONNA_INLINE
curve25519_add(bignum25519 out, const bignum25519 a, const bignum25519 b) {
	out[0] = a[0] + b[0];
	out[1] = a[1] + b[1];
	out[2] = a[2] + b[2];
	out[3] = a[3] + b[3];
	out[4] = a[4] + b[4];
}

static void DONNA_INLINE
curve25519_add_reduce(bignum25519 out, const bignum25519 a, const bignum25519 b) {
	uint64_t c;
	out[0] = a[0] + b[0]    ; c = (out[0] >> 51); out[0] &= reduce_mask_51;
	out[1] = a[1] + b[1] + c; c = (out[1] >> 51); out[1] &= reduce_mask_51;
	out[2] = a[2] + b[2] + c; c = (out[2] >> 51); out[2] &= reduce_mask_51;
	out[3] = a[3] + b[3] + c; c = (out[3] >> 51); out[3] &= reduce_mask_51;
	out[4] = a[4] + b[4] + c; c = (out[4] >> 51); out[4] &= reduce_mask_51;
	out[0] += c * 19;
}

static const uint64_t two54m152 = (((uint64_t)1) << 54) - 152;
static const uint64_t two54m8 = (((uint64_t)1) << 54) - 8;

/* out = a - b */
static void DONNA_INLINE
curve25519_sub(bignum25519 out, const bignum25519 a, const bignum25519 b) {
	out[0] = a[0] + two54m152 - b[0];
	out[1] = a[1] + two54m8 - b[1];
	out[2] = a[2] + two54m8 - b[2];
	out[3] = a[3] + two54m8 - b[3];
	out[4] = a[4] + two54m8 - b[4];
}

static void DONNA_INLINE
curve25519_sub_reduce(bignum25519 out, const bignum25519 a, const bignum25519 b) {
	uint64_t c;
	out[0] = a[0] + two54m152 - b[0]  ; c = (out[0] >> 51); out[0] &= reduce_mask_51;
	out[1] = a[1] + two54m8 - b[1] + c; c = (out[1] >> 51); out[1] &= reduce_mask_51;
	out[2] = a[2] + two54m8 - b[2] + c; c = (out[2] >> 51); out[2] &= reduce_mask_51;
	out[3] = a[3] + two54m8 - b[3] + c; c = (out[3] >> 51); out[3] &= reduce_mask_51;
	out[4] = a[4] + two54m8 - b[4] + c; c = (out[4] >> 51); out[4] &= reduce_mask_51;
	out[0] += c * 19;
}

/* out = a * b */
static void DONNA_INLINE
curve25519_mul(bignum25519 out, const bignum25519 in2, const bignum25519 in) {
#if !defined(HAVE_NATIVE_UINT128)
	uint128_t mul;
#endif
	uint128_t t[5];
	uint64_t r0,r1,r2,r3,r4,s0,s1,s2,s3,s4,c;

	r0 = in[0];
	r1 = in[1];
	r2 = in[2];
	r3 = in[3];
	r4 = in[4];

	s0 = in2[0];
	s1 = in2[1];
	s2 = in2[2];
	s3 = in2[3];
	s4 = in2[4];

#if defined(HAVE_NATIVE_UINT128)
	t[0]  =  ((uint128_t) r0) * s0;
	t[1]  =  ((uint128_t) r0) * s1 + ((uint128_t) r1) * s0;
	t[2]  =  ((uint128_t) r0) * s2 + ((uint128_t) r2) * s0 + ((uint128_t) r1) * s1;
	t[3]  =  ((uint128_t) r0) * s3 + ((uint128_t) r3) * s0 + ((uint128_t) r1) * s2 + ((uint128_t) r2) * s1;
	t[4]  =  ((uint128_t) r0) * s4 + ((uint128_t) r4) * s0 + ((uint128_t) r3) * s1 + ((uint128_t) r1) * s3 + ((uint128_t) r2) * s2;
#else
	mul64x64_128(t[0], r0, s0)
	mul64x64_128(t[1], r0, s1) mul64x64_128(mul, r1, s0) add128(t[1], mul)
	mul64x64_128(t[2], r0, s2) mul64x64_128(mul, r2, s0) add128(t[2], mul) mul64x64_128(mul, r1, s1) add128(t[2], mul)
	mul64x64_128(t[3], r0, s3) mul64x64_128(mul, r3, s0) add128(t[3], mul) mul64x64_128(mul, r1, s2) add128(t[3], mul) mul64x64_128(mul, r2, s1) add128(t[3], mul)
	mul64x64_128(t[4], r0, s4) mul64x64_128(mul, r4, s0) add128(t[4], mul) mul64x64_128(mul, r3, s1) add128(t[4], mul) mul64x64_128(mul, r1, s3) add128(t[4], mul) mul64x64_128(mul, r2, s2) add128(t[4], mul)
#endif

	r1 *= 19;
	r2 *= 19;
	r3 *= 19;
	r4 *= 19;

#if defined(HAVE_NATIVE_UINT128)
	t[0] += ((uint128_t) r4) * s1 + ((uint128_t) r1) * s4 + ((uint128_t) r2) * s3 + ((uint128_t) r3) * s2;
	t[1] += ((uint128_t) r4) * s2 + ((uint128_t) r2) * s4 + ((uint128_t) r3) * s3;
	t[2] += ((uint128_t) r4) * s3 + ((uint128_t) r3) * s4;
	t[3] += ((uint128_t) r4) * s4;
#else
	mul64x64_128(mul, r4, s1) add128(t[0], mul) mul64x64_128(mul, r1, s4) add128(t[0], mul) mul64x64_128(mul, r2, s3) add128(t[0], mul) mul64x64_128(mul, r3, s2) add128(t[0], mul)
	mul64x64_128(mul, r4, s2) add128(t[1], mul) mul64x64_128(mul, r2, s4) add128(t[1], mul) mul64x64_128(mul, r3, s3) add128(t[1], mul)
	mul64x64_128(mul, r4, s3) add128(t[2], mul) mul64x64_128(mul, r3, s4) add128(t[2], mul)
	mul64x64_128(mul, r4, s4) add128(t[3], mul)
#endif


	                     r0 = lo128(t[0]) & 0x7ffffffffffff; shr128(c, t[0], 51);
	add128_64(t[1], c)   r1 = lo128(t[1]) & 0x7ffffffffffff; shr128(c, t[1], 51);
	add128_64(t[2], c)   r2 = lo128(t[2]) & 0x7ffffffffffff; shr128(c, t[2], 51);
	add128_64(t[3], c)   r3 = lo128(t[3]) & 0x7ffffffffffff; shr128(c, t[3], 51);
	add128_64(t[4], c)   r4 = lo128(t[4]) & 0x7ffffffffffff; shr128(c, t[4], 51);
	r0 +=   c * 19; c = r0 >> 51; r0 = r0 & 0x7ffffffffffff;
	r1 +=   c;      c = r1 >> 51; r1 = r1 & 0x7ffffffffffff;
	r2 +=   c;

	out[0] = r0;
	out[1] = r1;
	out[2] = r2;
	out[3] = r3;
	out[4] = r4;
}

static void
curve25519_mul_noinline(bignum25519 out, const bignum25519 in2, const bignum25519 in) {
	curve25519_mul(out, in2, in);
}


/* out = in^(2 * count) */
static void DONNA_INLINE
curve25519_square_times(bignum25519 out, const bignum25519 in, uint64_t count) {
#if !defined(HAVE_NATIVE_UINT128)
	uint128_t mul;
#endif
	uint128_t t[5];
	uint64_t r0,r1,r2,r3,r4,c;
	uint64_t d0,d1,d2,d4,d419;

	r0 = in[0];
	r1 = in[1];
	r2 = in[2];
	r3 = in[3];
	r4 = in[4];

	do {
		d0 = r0 * 2;
		d1 = r1 * 2;
		d2 = r2 * 2 * 19;
		d419 = r4 * 19;
		d4 = d419 * 2;

#if defined(HAVE_NATIVE_UINT128)
		t[0] = ((uint128_t) r0) * r0 + ((uint128_t) d4) * r1 + (((uint128_t) d2) * (r3     ));
		t[1] = ((uint128_t) d0) * r1 + ((uint128_t) d4) * r2 + (((uint128_t) r3) * (r3 * 19));
		t[2] = ((uint128_t) d0) * r2 + ((uint128_t) r1) * r1 + (((uint128_t) d4) * (r3     ));
		t[3] = ((uint128_t) d0) * r3 + ((uint128_t) d1) * r2 + (((uint128_t) r4) * (d419   ));
		t[4] = ((uint128_t) d0) * r4 + ((uint128_t) d1) * r3 + (((uint128_t) r2) * (r2     ));
#else
		mul64x64_128(t[0], r0, r0) mul64x64_128(mul, d4, r1) add128(t[0], mul) mul64x64_128(mul, d2,      r3) add128(t[0], mul)
		mul64x64_128(t[1], d0, r1) mul64x64_128(mul, d4, r2) add128(t[1], mul) mul64x64_128(mul, r3, r3 * 19) add128(t[1], mul)
		mul64x64_128(t[2], d0, r2) mul64x64_128(mul, r1, r1) add128(t[2], mul) mul64x64_128(mul, d4,      r3) add128(t[2], mul)
		mul64x64_128(t[3], d0, r3) mul64x64_128(mul, d1, r2) add128(t[3], mul) mul64x64_128(mul, r4,    d419) add128(t[3], mul)
		mul64x64_128(t[4], d0, r4) mul64x64_128(mul, d1, r3) add128(t[4], mul) mul64x64_128(mul, r2,      r2) add128(t[4], mul)
#endif

		                     r0 = lo128(t[0]) & 0x7ffffffffffff; shr128(c, t[0], 51);
		add128_64(t[1], c)   r1 = lo128(t[1]) & 0x7ffffffffffff; shr128(c, t[1], 51);
		add128_64(t[2], c)   r2 = lo128(t[2]) & 0x7ffffffffffff; shr128(c, t[2], 51);
		add128_64(t[3], c)   r3 = lo128(t[3]) & 0x7ffffffffffff; shr128(c, t[3], 51);
		add128_64(t[4], c)   r4 = lo128(t[4]) & 0x7ffffffffffff; shr128(c, t[4], 51);
		r0 +=   c * 19; c = r0 >> 51; r0 = r0 & 0x7ffffffffffff;
		r1 +=   c;      c = r1 >> 51; r1 = r1 & 0x7ffffffffffff;
		r2 +=   c;
	} while(--count);

	out[0] = r0;
	out[1] = r1;
	out[2] = r2;
	out[3] = r3;
	out[4] = r4;
}

static void
curve25519_square(bignum25519 out, const bignum25519 in) {
#if !defined(HAVE_NATIVE_UINT128)
	uint128_t mul;
#endif
	uint128_t t[5];
	uint64_t r0,r1,r2,r3,r4,c;
	uint64_t d0,d1,d2,d4,d419;

	r0 = in[0];
	r1 = in[1];
	r2 = in[2];
	r3 = in[3];
	r4 = in[4];

	d0 = r0 * 2;
	d1 = r1 * 2;
	d2 = r2 * 2 * 19;
	d419 = r4 * 19;
	d4 = d419 * 2;

#if defined(HAVE_NATIVE_UINT128)
	t[0] = ((uint128_t) r0) * r0 + ((uint128_t) d4) * r1 + (((uint128_t) d2) * (r3     ));
	t[1] = ((uint128_t) d0) * r1 + ((uint128_t) d4) * r2 + (((uint128_t) r3) * (r3 * 19));
	t[2] = ((uint128_t) d0) * r2 + ((uint128_t) r1) * r1 + (((uint128_t) d4) * (r3     ));
	t[3] = ((uint128_t) d0) * r3 + ((uint128_t) d1) * r2 + (((uint128_t) r4) * (d419   ));
	t[4] = ((uint128_t) d0) * r4 + ((uint128_t) d1) * r3 + (((uint128_t) r2) * (r2     ));
#else
	mul64x64_128(t[0], r0, r0) mul64x64_128(mul, d4, r1) add128(t[0], mul) mul64x64_128(mul, d2,      r3) add128(t[0], mul)
	mul64x64_128(t[1], d0, r1) mul64x64_128(mul, d4, r2) add128(t[1], mul) mul64x64_128(mul, r3, r3 * 19) add128(t[1], mul)
	mul64x64_128(t[2], d0, r2) mul64x64_128(mul, r1, r1) add128(t[2], mul) mul64x64_128(mul, d4,      r3) add128(t[2], mul)
	mul64x64_128(t[3], d0, r3) mul64x64_128(mul, d1, r2) add128(t[3], mul) mul64x64_128(mul, r4,    d419) add128(t[3], mul)
	mul64x64_128(t[4], d0, r4) mul64x64_128(mul, d1, r3) add128(t[4], mul) mul64x64_128(mul, r2,      r2) add128(t[4], mul)
#endif

		                    r0 = lo128(t[0]) & reduce_mask_51; shr128(c, t[0], 51);
	add128_64(t[1], c)   r1 = lo128(t[1]) & reduce_mask_51; shr128(c, t[1], 51);
	add128_64(t[2], c)   r2 = lo128(t[2]) & reduce_mask_51; shr128(c, t[2], 51);
	add128_64(t[3], c)   r3 = lo128(t[3]) & reduce_mask_51; shr128(c, t[3], 51);
	add128_64(t[4], c)   r4 = lo128(t[4]) & reduce_mask_51; shr128(c, t[4], 51);
	r0 +=   c * 19; c = r0 >> 51; r0 = r0 & reduce_mask_51;
	r1 +=   c;      c = r1 >> 51; r1 = r1 & reduce_mask_51;
	r2 +=   c;

	out[0] = r0;
	out[1] = r1;
	out[2] = r2;
	out[3] = r3;
	out[4] = r4;
}

/* Take a little-endian, 32-byte number and expand it into polynomial form */
static void DONNA_INLINE
curve25519_expand(bignum25519 out, const unsigned char *in) {
	uint64_t t;
	unsigned i;

	#define read51full(n,start,shift) \
		for (t = in[(start)] >> (shift), i = 0; i < (6 + ((shift)/6)); i++) \
			t |= ((uint64_t)in[i+(start)+1] << ((i * 8) + (8 - (shift)))); \
		out[n] = t & 0x7ffffffffffff;
	#define read51(n) read51full(n,(n*51)/8,(n*3)&7)

	read51(0)
	read51(1)
	read51(2)
	read51(3)
	read51(4)
}

/* Take a fully reduced polynomial form number and contract it into a
 * little-endian, 32-byte array
 */
static void DONNA_INLINE
curve25519_contract(unsigned char *out, const bignum25519 input) {
	uint64_t t[5];
	uint64_t f, i;

	t[0] = input[0];
	t[1] = input[1];
	t[2] = input[2];
	t[3] = input[3];
	t[4] = input[4];

	#define curve25519_contract_carry() \
		t[1] += t[0] >> 51; t[0] &= 0x7ffffffffffff; \
		t[2] += t[1] >> 51; t[1] &= 0x7ffffffffffff; \
		t[3] += t[2] >> 51; t[2] &= 0x7ffffffffffff; \
		t[4] += t[3] >> 51; t[3] &= 0x7ffffffffffff;

	#define curve25519_contract_carry_full() curve25519_contract_carry() \
		t[0] += 19 * (t[4] >> 51); t[4] &= 0x7ffffffffffff;

	#define curve25519_contract_carry_final() curve25519_contract_carry() \
		t[4] &= 0x7ffffffffffff;

	curve25519_contract_carry_full()
	curve25519_contract_carry_full()

	/* now t is between 0 and 2^255-1, properly carried. */
	/* case 1: between 0 and 2^255-20. case 2: between 2^255-19 and 2^255-1. */
	t[0] += 19;
	curve25519_contract_carry_full()

	/* now between 19 and 2^255-1 in both cases, and offset by 19. */
	t[0] += 0x8000000000000 - 19;
	t[1] += 0x8000000000000 - 1;
	t[2] += 0x8000000000000 - 1;
	t[3] += 0x8000000000000 - 1;
	t[4] += 0x8000000000000 - 1;

	/* now between 2^255 and 2^256-20, and offset by 2^255. */
	curve25519_contract_carry_final()

	#define write51full(n,shift) \
		f = ((t[n] >> shift) | (t[n+1] << (51 - shift))); \
		for (i = 0; i < 8; i++, f >>= 8) *out++ = (unsigned char)f;
	#define write51(n) write51full(n,13*n)
	write51(0)
	write51(1)
	write51(2)
	write51(3)
}

/* out = (flag) ? in : out */
static void DONNA_INLINE
curve25519_move_conditional(bignum25519 out, const bignum25519 in, uint64_t flag) {
	const uint64_t nb = flag - 1, b = ~nb;
	out[0] = (out[0] & nb) | (in[0] & b);
	out[1] = (out[1] & nb) | (in[1] & b);
	out[2] = (out[2] & nb) | (in[2] & b);
	out[3] = (out[3] & nb) | (in[3] & b);
	out[4] = (out[4] & nb) | (in[4] & b);
}

/* if (iswap) swap(a, b) */
static void DONNA_INLINE
curve25519_swap_conditional(bignum25519 a, bignum25519 b, uint64_t iswap) {
	const uint64_t swap = (uint64_t)(-(int64_t)iswap);
	uint64_t x0,x1,x2,x3,x4;

	x0 = swap & (a[0] ^ b[0]); a[0] ^= x0; b[0] ^= x0;
	x1 = swap & (a[1] ^ b[1]); a[1] ^= x1; b[1] ^= x1;
	x2 = swap & (a[2] ^ b[2]); a[2] ^= x2; b[2] ^= x2;
	x3 = swap & (a[3] ^ b[3]); a[3] ^= x3; b[3] ^= x3;
	x4 = swap & (a[4] ^ b[4]); a[4] ^= x4; b[4] ^= x4;
}

#define ED25519_64BIT_TABLES

