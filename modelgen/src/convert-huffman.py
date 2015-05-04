import sys
import json
import subprocess

def huffman(content):
  print(content)
  print('Writing frequencies...')
  f=open('tmp.freqs', 'w')
  for x in range(len(content)):
    count=int(content[x]*1000000)
    f.write(str(count))
    f.write("\n")
    print(str(x)+"/"+str(len(content))+' '+str(content[x])+' '+str(count))
  f.close()
  print('Generating code table...')
  subprocess.call(['/Users/brandon/Dust-tools/dist/build/huffman/huffman', 'tmp.freqs', 'tmp.huffman'])

  f=open('tmp.huffman')
  s=f.read()
  f.close()

  l=s.split("\n")
  l=map(parse, l)
  l=filter(lambda x: x!=None, l)

  print(l)

  return l

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

f=open(sys.argv[1])
s=f.read()
f.close()
data=json.loads(s)

data['incomingModel']['huffman']=huffman(data['incomingModel']['content']['params'])
data['outgoingModel']['huffman']=huffman(data['outgoingModel']['content']['params'])

f=open(sys.argv[1], 'w')
f.write(json.dumps(data))
f.close()
