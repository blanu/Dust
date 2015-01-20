import yaml

def parse(item):
  if item=='':
    return None
  a=[]
  for x in item:
    if x=="0":
      a.append(False)
    elif x=="1":
      a.append(True)
  return a

f=open('huffman.export')
s=f.read()
f.close()

l=s.split("\n")
l=map(parse, l)
l=filter(lambda x: x!=None, l)

f=open('src/Dust/models/test.yaml')
s=f.read()
f.close()

data=yaml.load(s)
data['huffman']=l

f=open('src/Dust/models/test.yaml', 'w')
f.write(yaml.dump(data))
f.close()
