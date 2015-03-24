import sys
import json

f=open(sys.argv[1])
s=f.read()
f.close()

seqs=json.loads(s)

f=open(sys.argv[2])
s=f.read()
f.close()

data=json.loads(s)
data['incomingModel']['sequence']=seqs['incoming']
data['outgoingModel']['sequence']=seqs['outgoing']

f=open(sys.argv[2], 'w')
f.write(json.dumps(data))
f.close()
