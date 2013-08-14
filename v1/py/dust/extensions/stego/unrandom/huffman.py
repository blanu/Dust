# Copyright (c) 2011 the authors listed at the following URL, and/or
# the authors of referenced articles or incorporated external code:
# http://en.literateprograms.org/Huffman_coding_(Python)?action=history&offset=20081223225116
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
# Retrieved from: http://en.literateprograms.org/Huffman_coding_(Python)?oldid=15672

import heapq

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

exampleData = [
  (0.124167  , 'e'),   
  (0.0969225 , 't'),   
  (0.0820011 , 'a'),   
  (0.0768052 , 'i'),   
  (0.0764055 , 'n'),   
  (0.0714095 , 'o'),   
  (0.0706768 , 's'),   
  (0.0668132 , 'r'),   
  (0.0448308 , 'l'),   
  (0.0363709 , 'd'),   
  (0.0350386 , 'h'),   
  (0.0344391 , 'c'),   
  (0.028777  , 'u'),   
  (0.0281775 , 'm'),   
  (0.0235145 , 'f'),   
  (0.0203171 , 'p'),   
  (0.0189182 , 'y'),   
  (0.0181188 , 'g'),   
  (0.0135225 , 'w'),   
  (0.0124567 , 'v'),   
  (0.0106581 , 'b'),   
  (0.00393019, 'k'),   
  (0.00219824, 'x'),   
  (0.0019984 , 'j'),   
  (0.0009325 , 'q'),   
  (0.000599  , 'z')   
]


if __name__ == '__main__':
   huffTree = makeHuffTree(exampleData)
   printHuffTree(huffTree)

