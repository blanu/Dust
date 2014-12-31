import os
import yaml
import json
import subprocess
from airspeed import CachingFileLoader

def parse(filename):
  f=open(filename)
  data=f.read()
  f.close()

  ext=filename.split('.')[1]
  if ext=='yaml':
    obj=yaml.load(data)
  elif ext=="json":
    obj=json.loads(data)
  else:
    print('Unknown file extension '+ext)
    obj=None
  return obj

def save(data, filename):
  f=open(filename, 'w')
  f.write(data)
  f.close()

def convertModel(name, data):
  model={}
  model['name']=name.capitalize()
  model['duration']=genDuration(data['duration']['dist'], data['duration']['params'])
  model['packet_length']=genLength(data['incomingModel']['length']['dist'], data['incomingModel']['length']['params'])
  model['packet_count']=genFlow(data['incomingModel']['flow']['dist'], data['incomingModel']['flow']['params'])
  model['encode']=genEncoder(data['incomingModel']['huffman'])
  model['decode']=genDecoder()
  model['random_bytes']=genContent(data['incomingModel']['content']['dist'], data['incomingModel']['content']['params'])
  model['add_padding']={'body': 'return bytes'}
  model['strip_padding']={'body': 'return bytes'}

  return model

def genDuration(dist, params):
  if dist=='exponential':
    return genExponential("durationDist", params[0])
  else:
    print('Unknown duration dist %s' % dist)
    return None

def genExponential(varname, param):
  return {
    'data': "var %s=&dist.Exponential{Rate: float64(%s)}" % (varname, param),
    'body': "return uint16(%s.Rand())" % (varname)
  }

def genLength(dist, params):
  if dist=='normal':
    return genNormal("lengthDist", params[0], params[1])
  else:
    print('Unknown length dist %s' % dist)
    return None

def genNormal(varname, mu, sigma):
  return {
    'data': "var %s=dist.Normal{Mu: float64(%f), Sigma: float64(%f)}" % (varname, mu, sigma),
    'body': "return uint16(%s.Rand())" % (varname)
  }

def genFlow(dist, params):
  if dist=='poisson':
    return genPoisson("flowDist", params[0])
  else:
    print('Unknown flow dist %s' % dist)
    return None

# FIXME - Find code for generating a proper Poisson distribution
def genPoisson(varname, param):
  return {
    'data': "var %s=&dist.Poisson{Expected: float64(%f)}" % (varname, param),
    'body': "return uint16(%s.Rand())" % (varname)
  }

def genContent(dist, params):
  if dist=='multinomial':
    return genMultinomial("contentDist", params)
  else:
    print('Unknown content dist %s' % dist)

def genMultinomial(varname, params):
  return {
    'data': "var %sWeights=[]float64 %s\nvar %s=&dist.Multinomial{Weights: %sWeights}" % (varname, genArray(convertToProbs(params)), varname, varname),
    'body': """
      var bytes=make([]byte, requestedLength)
      for index := range bytes {
        bytes[index]=byte(%s.Rand())
      }

      return bytes
      """
      % (varname)
  }

def convertToProbs(arr):
  total=float(sum(arr))
  return map(lambda item: float(item)/total, arr)

def genEncoder(params):
  varname="contentDist"
  return {
    'data': "var huffman=[][]bool %s\nvar encoder *HuffmanEncoder=nil" % (genBoolArrays(params)),
    'body': """
      if encoder==nil {
        encoder=GenerateHuffmanEncoder(huffman)
        print(encoder.String())
      }

      return encoder.encode(bytes)
      """
  }

def genDecoder():
  varname="contentDist"
  return {
    'body': """
      if encoder==nil {
        encoder=GenerateHuffmanEncoder(huffman)
        print(encoder.String())
      }

      return encoder.decode(bytes)
      """
  }

def genArray(arr):
  return '{'+', '.join(map(str, arr))+'}'

def genBoolArrays(arr):
  return '{'+', '.join(map(genBoolArray, arr))+'}'

def genBoolArray(arr):
  return '{'+', '.join(map(boolstr, arr))+'}'

def boolstr(b):
  if b:
    return 'true'
  else:
    return 'false'

templateName='model.go.airspeed'
testTemplateName='test.go.airspeed'

models=os.listdir('models')
for modelName in models:
  print(modelName)
  modelBasename=modelName.split('.')[0]
  modelFilename='models/'+modelName
  outputName=modelBasename+'.go'
  testOutputName=modelBasename+'_test.go'

  loader = CachingFileLoader('templates')

  model=parse(modelFilename)
  context=convertModel(modelBasename, model)

  print("Generating %s" % (modelBasename+'.go'))
  template = loader.load_template(templateName)
  body=template.merge(context, loader=loader)
  save(body, outputName)
  print("Formatting %s" % (modelBasename+'.go'))
  subprocess.call(['gofmt', '-s=true', '-w=true', outputName])

  print("Generating %s" % (modelBasename+'_test.go'))
  template = loader.load_template(testTemplateName)
  body=template.merge(context, loader=loader)
  save(body, testOutputName)
  print("Formatting %s" % (modelBasename+'_test.go'))
  subprocess.call(['gofmt', '-s=true', '-w=true', testOutputName])
