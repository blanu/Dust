from encode import WordsEncoding

bs=b'\x00\x01\xFF'

w=WordsEncoding()
words=w.encode(bs)
text=' '.join(words)
words2=text.split(' ')
bs2=w.decode(words2)

print(bs)
print(words)
print(text)
print(words2)
print(bs2)