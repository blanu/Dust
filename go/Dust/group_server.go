package Dust

import (
	"github.com/blanu/Dust/go/Dust/proc"
)

// ServerListener indicates how to accept new connections for the server side of a Dust connection group.
type ServerListener interface {
	Accept() (Socket, error)
}

// serverGroup is a group that can passively add new connections from outside by accepting them.
type serverGroup struct {
	group

	srvLink proc.Link

	spriv    *ServerPrivate
	listener ServerListener
}

func (sg *serverGroup) initServerGroup(spriv *ServerPrivate, listener ServerListener) {
	sg.initGroup()
	sg.srvLink.InitLink(sg.runAccepting)
	sg.spriv = spriv
	sg.listener = listener
	// No need to set sg.discipline; the null discipline is fine.
}

func (sg *serverGroup) runAccepting() error {
	for {
		gconn := &groupedConnection{}
		gconn.initGroupedConnection()

		err := gconn.initServer(sg.spriv, gconn)
		if err != nil {
			return err
		}

		sock, err := sg.listener.Accept()
		if err != nil {
			return err
		}

		sg.inConns <- gconn

		go func() {
			started := false
			defer func() {
				if !started {
					gconn.Closed()
					sock.Close()
				}
			}()

			select {
			case _ = <-sg.srvLink.Kill:
				return
			case _ = <-gconn.sigStart:
			}

			err := gconn.spawn(sock)
			started = err == nil
		}()
	}
}

func (sg *serverGroup) spawn() {
	sg.groupLink.Spawn()
	sg.srvLink.Spawn()
}
