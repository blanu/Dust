import sys
import heapq
from bitstring import ConstBitStream, BitArray

def makeHuffTree(symbolTupleList):
   trees = list(symbolTupleList)

   heapq.heapify(trees)
   while len(trees) > 1:
      childR, childL = heapq.heappop(trees), heapq.heappop(trees)
      parent = (childL[0] + childR[0], childL, childR)
      heapq.heappush(trees, parent)

   return trees[0]

def printHuffTree(huffTree, prefix = ''):
   if len(huffTree) == 2:
      print huffTree[1], prefix

   else:
      printHuffTree(huffTree[1], prefix + '0')
      printHuffTree(huffTree[2], prefix + '1')

def createDecoding(huffTree, prefix = '', map={}):
   if len(huffTree) == 2:
      map[huffTree[1]]=prefix
   else:
      createDecoding(huffTree[1], prefix + '0', map)
      createDecoding(huffTree[2], prefix + '1', map)
   return map

class Encoder:
  def __init__(self, dist):
    self.dist=dist
    self.encoding=makeHuffTree(dist)
    self.decoding=createDecoding(self.encoding)
    printHuffTree(self.encoding)

  def encode(self, infile, outfile):
    s=''
    chars=[]

    print(BitArray(filename=infile).bin[2:])

    f=ConstBitStream(filename=infile)
    done=False
    eof=False
    while not done:
      found=False
      cursor=self.encoding
      while not found:
        try:
          bit=f.read('uint:1')
        except:
          eof=True
          bit=0
        cursor=cursor[bit+1]
        if len(cursor)==2: # leaf
          found=True
          val=cursor[1]
          s=s+chr(val)
          chars.append(val)
          if eof:
            done=True

    f=open(outfile, 'wb')
    f.write(s)
    f.close()

    print(chars)
    print(BitArray(filename=outfile).bin[2:])

  def decode(self, infile, outfile):
    print(BitArray(filename=infile).bin[2:])

    f=open(infile, 'rb')
    bs=f.read()
    f.close()

    s=''
    for b in bs:
      s=s+self.decoding[ord(b)]

    while(len(s)%8!=0):
      s=s[:-1]

    print(s)

    f=open(outfile, 'wb')
    f.write(BitArray('0b'+s).bytes)
    f.close()
