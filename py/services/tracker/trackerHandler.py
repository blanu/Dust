from util.ymap import YamlMap
from services.tracker.trackbackClient import TrackbackClient

class TrackerHandler:
  def __init__(self, router, addr):
    print('new TrackerHandler '+str(addr))
    self.router=router
    self.addr=addr
    self.state=YamlMap('config/tracker.yaml')

  def getPeerForEndpoint(self, key):
    print('getPeerForEndpoint('+str(key)+')')
    try:
      map=self.state['endpoints']
      value=map[key]
      trackback=TrackbackClient(self.router, self.addr)
      trackback.putPeerForEndpoint(key, value)
    except:
      print('exception')
      pass

  def putPeerForEndpoint(self, key, value):
    print('putPeerForEndpoint('+str(key)+','+str(value)+')')
    try:
      map=self.state['endpoints']
      map[key]=value
      self.state.save()
    except:
      map={key: value}
      self.state['endpoints']=map
