import os
import yaml
import json
import subprocess
import re
from airspeed import CachingFileLoader

def parse(filename):
  print('Parsing '+filename)
  f=open(filename)
  data=f.read()
  f.close()

  ext=filename.split('/')[-1].split('.')[1]
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
  model['packagename']=name
  model['name']=name.capitalize()
  model['duration']=genDuration(data['duration']['dist'], data['duration']['params'])
  model['packet_length']=genLength(data['incomingModel']['length']['dist'], data['incomingModel']['length']['params'])
  model['packet_count']=genFlow(data['incomingModel']['flow']['dist'], data['incomingModel']['flow']['params'])
  model['huffman']=genHuffman(data['incomingModel']['huffman'])
  model['encode']=genEncoder(data['incomingModel']['huffman'])
  model['decode']=genDecoder()
  model['weights']=genWeights("contentDist", data['incomingModel']['content']['params'])
  model['random_bytes']=genContent(data['incomingModel']['content']['dist'], data['incomingModel']['content']['params'])

  return model

def genDuration(dist, params):
  if dist=='exponential':
    return genExponential("durationDist", params[0])
  else:
    print('Unknown duration dist %s' % dist)
    return None

def genExponential(varname, param):
  return {
    'decl': "%s dist.Exponential" % (varname),
    'data': "%s: dist.Exponential{Rate: float64(%s)}," % (varname, param),
    'body': "return uint16(self.%s.Rand())" % (varname)
  }

def genLength(dist, params):
  if dist=='normal':
    return genNormal("lengthDist", params[0], params[1])
  else:
    print('Unknown length dist %s' % dist)
    return None

def genNormal(varname, mu, sigma):
  return {
    'decl': "%s dist.Normal" % (varname),
    'data': "%s: dist.Normal{Mu: float64(%f), Sigma: float64(%f)}," % (varname, mu, sigma),
    'body': "return uint16(self.%s.Rand())" % (varname)
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
    'decl': "%s dist.Poisson" % (varname),
    'data': "%s: dist.Poisson{Expected: float64(%f)}," % (varname, param),
    'body': "var total uint16 = 0\nfor iteration := 0; iteration < int(milliseconds); iteration++ {\n  total=total+uint16(self.%s.Rand())\n}\n\nreturn total" % (varname)
  }

def genContent(dist, params):
  if dist=='multinomial':
    return genMultinomial("contentDist", params)
  else:
    print('Unknown content dist %s' % dist)

def genWeights(varname, params):
  return "var %sWeights = []float64 %s" % (varname, genArray(convertToProbs(params)))

def genMultinomial(varname, params):
  return {
    'decl': "%s dist.Multinomial" % (varname),
    'data': "%s: dist.Multinomial{Weights: %sWeights}," % (varname, varname),
    'body': """
      var bytes=make([]byte, requestedLength)
      for index := range bytes {
        bytes[index]=byte(self.%s.Rand())
      }

      return bytes
      """
      % (varname)
  }

def convertToProbs(arr):
  total=float(sum(arr))
  return map(lambda item: float(item)/total, arr)

def genHuffman(params):
  return "var huffmanCodes = [][]bool %s" % (genBoolArrays(params))

def genEncoder(params):
  varname="contentDist"
  return {
    'decl': "encoder *huffman.HuffmanEncoder",
    'data': "encoder: huffman.GenerateHuffmanEncoder(huffmanCodes)",
    'body': """
      print(self.encoder.String())

      return self.encoder.Encode(bytes)
      """
  }

def genDecoder():
  varname="contentDist"
  return {
    'body': """
      print(self.encoder.String())

      return self.encoder.Decode(bytes)
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
loader = CachingFileLoader('templates')

def looksLikeModelFile(name):
  return re.search(r'\.(?:yaml|json)\Z', name)

modelDir='./data'
models=filter(looksLikeModelFile, os.listdir(modelDir))
modelBases=[]
for modelName in models:
  print(modelName)
  modelBasename=modelName.split('.')[0]
  modelBases.append(modelBasename)
  modelFilename=modelDir+'/'+modelName
  outputDir='generated/'+modelBasename
  if not os.path.exists(outputDir):
    os.mkdir(outputDir)
  outputName=outputDir+'/'+modelBasename+'.go'
  testOutputName=outputDir+'/'+modelBasename+'_test.go'

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

context={'models': modelBases}
print("Generating models.go")
template = loader.load_template("models.go.airspeed")
body=template.merge(context, loader=loader)
save(body, "models.go")
print("Formatting models.go")
subprocess.call(['gofmt', '-s=true', '-w=true', "models.go"])

context={'models': modelBases}
print("Generating models_test.go")
template = loader.load_template("models_test.go.airspeed")
body=template.merge(context, loader=loader)
save(body, "models_test.go")
print("Formatting models_test.go")
subprocess.call(['gofmt', '-s=true', '-w=true', "models_test.go"])
