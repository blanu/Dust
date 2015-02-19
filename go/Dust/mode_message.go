package Dust

type MessageParams struct {
}

type MessageEndpoint struct {
	DustPacketConn
	MessageParams
}

func BeginMessageClient(
	dialer ClientDialer,
	spub *ServerPublic,
	params MessageParams,
) (mep *MessageEndpoint, err error) {
	cg := &clientGroup{}
	cg.initClientGroup(spub, dialer)
	cg.spawn()
	mep = &MessageEndpoint{DustPacketConn: cg, MessageParams: params}
	return
}

func BeginMessageServer(
	listener ServerListener,
	spriv *ServerPrivate,
	params MessageParams,
) (mep *MessageEndpoint, err error) {
	sg := &serverGroup{}
	sg.initServerGroup(spriv, listener)
	sg.spawn()
	mep = &MessageEndpoint{DustPacketConn: sg, MessageParams: params}
	return
}
