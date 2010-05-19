import yaml

class WordsEncoding:
  def __init__(self, filename='words.yaml'):
    f=open(filename, 'r')
    self.words=yaml.load(f.read())
    f.close()
    
  def encode(self, bs):
    results=[]
    for b in bs:
      results.append(self.words[b])
    return results
    
  def decode(self, ws):
    results=bytearray()
    for w in ws:
      b=self.words.index(w)
      results.append(b)
    return bytes(results)
    