from dust.util.ymap import YamlMap

class TrackbackHandler:
  def __init__(self, router):
    self.state=YamlMap('config/trackback.yaml')

  def putPeerForEndpoint(self, key, value):
    try:
      map=self.state['endpoints']
      map[key]=value
      self.state.save()
    except:
      pass
