from dust.util.ymap import YamlMap
from dust.invite.invite_packet import InviteMessage
from dust.core.util import decode

class TrackbackHandler:
  def __init__(self, keys):
    self.keys=keys
    self.state=YamlMap('config/trackback.yaml')
    self.callbacks={}

  def setPutPeerForEndpointCallback(self, key, callback):
    self.callbacks[key]=callback

  def putPeerForEndpoint(self, key, value):
    print('putPeerForEndpoint!!! '+str(key)+' '+str(value))
    try:
      map=self.state['endpoints']
      map[key]=value
      self.state.save()
    except:
      map={key: value}
      self.state['endpoints']=map
    print('callbacks: '+str(self.callbacks))
    callback=self.callbacks[key]
    callback(key, value)

  def setPutInviteForPeer(self, key, callback):
    self.callbacks[key]=callback

  def putInviteForPeer(self, addr, invite):
    print('putInviteForPeer!!! '+str(addr)+' '+str(invite))
    i=InviteMessage()
    i.decodeInviteMessage(decode(invite))
    self.keys.outgoingInvites.addInvite(i)
    if self.keys.outgoingFilename:
      self.keys.saveOutgoingInvites(self.keys.outgoingFilename, self.keys.invitePassword)

    callback=self.callbacks[addr]
    callback(addr, i)

  def setPutTrackerInvite(self, callback):
    self.callbacks['tracker']=callback

  def putTrackerInvite(self, invite):
    print('putTrackerInvite: '+str(invite))

    callback=self.callbacks['tracker']
    print('callback: '+str(callback))
    callback(decode(invite))
