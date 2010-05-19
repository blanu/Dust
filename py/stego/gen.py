import re
import yaml

wordlist=[]
f=open('words.txt', 'r')
while True:
  try:
    line=f.readline()
    #print('line:', line)
  except:
    print('Error reading line:', line)
    continue
  if line=='':
    break
  line=line.strip()
  #print('line:', line)
  words=re.findall('([A-Za-z]+)', line)
  #print('words:', words)
  for word in words:
    if len(word)>2 and not word in wordlist:
      wordlist.append(word)
f.close()
      
print(len(wordlist))
wordlist=wordlist[0:256]
print(len(wordlist))
print(wordlist)

f=open('words.yaml', 'w')
f.write(yaml.dump(wordlist))
f.close()
