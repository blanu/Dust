package DustCrypto

import (
	"encoding/base32"
	"errors"
	"strings"
	
	"code.google.com/p/go.crypto/curve25519"
	"github.com/agl/ed25519/extra25519"
)

// PublicKey represents a Curve25519 public key plus a uniform representative.
type PublicKey interface {
	Bytes() []byte
	UniformBytes() []byte
	Base32() string
}

type publicKey struct {
	actual [32]byte
	uniform [32]byte
}

// Bytes returns a byte slice with a representation of the public key.
func (public *publicKey) Bytes() []byte {
	return public.actual[:]
}

// UniformBytes returns a byte slice with the uniform representative of the public key.
func (public *publicKey) UniformBytes() []byte {
	return public.uniform[:]
}

// Base32 returns a string with the unpadded Base32 representation of the uniform representative of the public
// key.
func (public *publicKey) Base32() string {
	padded := base32.StdEncoding.EncodeToString(public.UniformBytes())
	unpadded := strings.TrimRight(padded, `=`)
	if len(unpadded) != 52 {
		panic("weird Base32 consistency error")
	}
	return unpadded
}

var (
	ErrBadPublicKey = errors.New("bad public key")
	ErrBadPrivateKey = errors.New("bad private key")
	ErrImprobableNonuniformity = errors.New("unreasonably long sequence of un-Elligatorable private keys")
)

// LoadPublicKeyUniform extracts and returns a public key from a 32-byte slice containing its uniform
// representative.
func LoadPublicKeyUniform(b []byte) (PublicKey, error) {
	if len(b) != 32 {
		return nil, ErrBadPublicKey
	}

	public := &publicKey{}
	copy(public.uniform[:], b)
	extra25519.RepresentativeToPublicKey(&public.actual, &public.uniform)
	return public, nil
}

// LoadPublicKeyBase32 extracts and returns a public key from an unpadded Base32 string containing its
// uniform representative.
func LoadPublicKeyBase32(s string) (PublicKey, error) {
	if len(s) != 52 {
		return nil, ErrBadPublicKey
	}

	b, err := base32.StdEncoding.DecodeString(strings.Join([]string{s, `====`}, ""))
	if err != nil {
		return nil, ErrBadPublicKey
	}

	return LoadPublicKeyUniform(b)
}

type privateKey SecretBytes

// KeyPair represents a Curve25519 private key along with its associated public key.
type KeyPair interface {
	Public() PublicKey
	DestroyPrivate()
	ECDH(public PublicKey, sbOut SecretBytes)
	PrivateSlice() []byte
	PrivateBase32() string
}

type keyPair struct {
	public PublicKey
	private privateKey
}

// Public returns the public key corresponding to a keypair.
func (kp *keyPair) Public() PublicKey {
	return kp.public
}

func (kp *keyPair) DestroyPrivate() {
	kp.private.(SecretBytes).Destroy()
}

// NewKeyPair generates a new Curve25519 keypair and returns it.
func NewKeyPair() (KeyPair, error) {
	privateRepr := NewSecretBytes()
	ok := false
	defer func() {
		if !ok {
			privateRepr.Destroy()
		}
	}()

	public := &publicKey{}
	acceptable := false
	
	for tries := 0; !acceptable && tries < 256; {
		err := RandomizeSecretBytes(privateRepr)
		if err != nil {
			return nil, err
		}

		privatePtr := privateRepr.Pointer()
		privatePtr[0] &= 248
		privatePtr[31] &= 127
		privatePtr[31] |= 64

		acceptable = extra25519.ScalarBaseMult(&public.actual, &public.uniform, privatePtr)
	}
	
	if !acceptable {
		return nil, ErrImprobableNonuniformity
	}
	
	private := privateKey(privateRepr)
	keypair := &keyPair{public, private}
	ok = true
	return keypair, nil
}

// ECDH computes an ECDH-derived raw value from a (local) keypair and a (remote) public key and
// overwrites sbOut with the result.
func (kp *keyPair) ECDH(public PublicKey, sbOut SecretBytes) {
	curve25519.ScalarMult(sbOut.Pointer(), kp.private.(SecretBytes).Pointer(),
		&public.(*publicKey).actual)
}

func LoadKeyPairOwningPrivate(raw SecretBytes) (KeyPair, error) {
	public := &publicKey{}
	acceptable := extra25519.ScalarBaseMult(&public.actual, &public.uniform, raw.Pointer())
	if !acceptable {
		return nil, ErrBadPrivateKey
	}

	private := privateKey(raw)
	return &keyPair{public, private}, nil
}

func (kp *keyPair) PrivateSlice() []byte {
	return kp.private.(SecretBytes).Slice()
}

func (kp *keyPair) PrivateBase32() string {
	padded := base32.StdEncoding.EncodeToString(kp.private.Slice())
	unpadded := strings.TrimRight(padded, `=`)
	if len(unpadded) != 52 {
		panic("weird Base32 consistency error")
	}
	return unpadded
}

func LoadPrivateKeyBase32(s string) (KeyPair, error) {
	if len(s) != 52 {
		return nil, ErrBadPrivateKey
	}

	b, err := base32.StdEncoding.DecodeString(strings.Join([]string{s, `====`}, ""))
	if err != nil || len(b) != 32 {
		return nil, ErrBadPrivateKey
	}

	raw := NewSecretBytes()
	copy(raw.Slice(), b)
	return LoadKeyPairOwningPrivate(raw)
}
