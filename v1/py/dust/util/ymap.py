import os
import yaml

# This is not thread safe
class YamlMap:
  def __init__(self, filename):
    self.filename=filename
    if os.path.exists(filename):
      f=open(self.filename, 'r')
      self.map=yaml.load(f)
      f.close()
    else:
      self.map={}

  def getWithDefault(self, key, default):
    if key in self.map.keys():
      return self.map[key]
    else:
      return default

  def __getitem__(self, key):
    return self.map[key]

  def __setitem__(self, key, value):
    self.map[key]=value
    self.save()

  def keys(self):
    return self.map.keys()

  def values(self):
    return self.map.values()

  def save(self):
    f=open(self.filename, 'w')
    yaml.dump(self.map, f)
    f.close()

