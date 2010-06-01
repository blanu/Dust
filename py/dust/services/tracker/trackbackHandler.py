from dust.util.ymap import YamlMap

class TrackbackHandler:
  def __init__(self):
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
