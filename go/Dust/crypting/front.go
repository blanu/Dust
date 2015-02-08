package crypting

import (
	"errors"
	"io"

	"github.com/op/go-logging"

	"github.com/blanu/Dust/go/Dust/buf"
)

var (
	ErrSomeDatagramsLost = errors.New("Dust/crypting: some datagrams lost")
)

type Front interface {
	PullGram(write func(p []byte, mayRetain bool))
	PushGram(p []byte, mayRetain bool)
	DrainOutput()
}

type numberedGram struct {
	seq  int64
	data []byte
}

type InvertingFront struct {
	Params

	// Datagrams with sequence numbers are sent to inPlains from the outward-facing side of the crypto
	// session.  The receiver owns the data chunks.  inSequence is the next sequence number to send from
	// the outward-facing side.  inLast is the last sequence number successfully delivered by the
	// inward-facing side, and inGramLeft buffers up to one datagram on the inward-facing side.  Sequence
	// numbers for actual datagrams start at 1.
	inGrams    chan numberedGram
	inSequence int64
	inLast     int64
	inLossage  int64
	inGramLeft numberedGram

	// Datagrams are sent to outPlains from the inward-facing side of the crypto session.  The receiver
	// owns the chunks.
	outPlains chan []byte
}

func (inv *InvertingFront) Init() {
	inv.inGrams = make(chan numberedGram, 4)
	inv.outPlains = make(chan []byte, 4)
	inv.inSequence = 1
}

func (inv *InvertingFront) PullGram(write func(p []byte, mayRetain bool)) {
	select {
	default:
		// Nothing available.
	case dgram := <-inv.outPlains:
		write(dgram, true)
	}
}

func (inv *InvertingFront) Write(p []byte) (n int, err error) {
	if log.IsEnabledFor(logging.DEBUG) {
		defer func() {
			log.Debug("-> %d plain bytes", n)
		}()
	}

	dgram := buf.CopyNew(p)
	if len(dgram) > inv.MTU {
		dgram = dgram[:inv.MTU]
		err = io.ErrShortWrite
	}
	inv.outPlains <- buf.CopyNew(dgram)
	return len(dgram), err
}

func (inv *InvertingFront) PushGram(p []byte, mayRetain bool) {
	if !mayRetain {
		p = buf.CopyNew(p)
	}

	ngram := numberedGram{inv.inSequence, p}
	inv.inSequence++
	select {
	case inv.inGrams <- ngram:
	default:
		// Inward-facing side not consuming these fast enough.  Drop the datagram on
		// the floor.
	}
}

func (inv *InvertingFront) Read(p []byte) (n int, err error) {
	if log.IsEnabledFor(logging.DEBUG) {
		defer func() {
			log.Debug("  <- %d plain bytes", n)
		}()
	}

	var ngram numberedGram
	if inv.inGramLeft.seq != 0 {
		ngram = inv.inGramLeft
		inv.inGramLeft = numberedGram{}
	} else {
		ngram = <-inv.inGrams
	}

	inv.inLossage = 0
	if ngram.seq == 0 {
		return 0, io.EOF
	} else if ngram.seq != inv.inLast+1 {
		inv.inLossage = ngram.seq - (inv.inLast + 1)
		inv.inLast = ngram.seq - 1
		err = ErrSomeDatagramsLost
	}

	dgram := ngram.data
	n = copy(p, dgram)
	if n < len(dgram) {
		err = io.ErrShortBuffer
	}
	return n, err
}

func (inv *InvertingFront) GetReadLossage() int {
	return int(inv.inLossage)
}

func (inv *InvertingFront) DrainOutput() {
	for {
		select {
		case _ = <-inv.outPlains:
		default:
			return
		}
	}
}

func NewInvertingFront(params Params) *InvertingFront {
	inv := &InvertingFront{Params: params}
	inv.Init()
	return inv
}
