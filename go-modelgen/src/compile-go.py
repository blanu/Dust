import os
import sys
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
  print("Saving %s" % (filename))
  f=open(filename, 'w')
  f.write(data)
  f.close()

def convertModel(name, data):
  model={}
  model['packagename']=name
  model['name']=name
  model['model_type']=name+'Model'
  model['codec_type']=name+'Codec'
  model['duration']=genDuration(data['duration']['dist'], data['duration']['params'])
  model['packet_length']=genLength(data['incomingModel']['length']['dist'], data['incomingModel']['length']['params'])
  model['packet_sleep']=genITA(data['incomingModel']['flow']['dist'], data['incomingModel']['flow']['params'])
  model['huffman']=genHuffman(data['incomingModel']['huffman'])
  model['encode']=genEncoder(data['incomingModel']['huffman'])
  model['decode']=genDecoder()
  model['weights']=genWeights("contentDist", data['incomingModel']['content']['params'])

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
    'data': "%s: dist.Exponential{Rate: float64(%s), Source: prng}," % (varname, param),
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
    'data': "%s: dist.Normal{Mu: float64(%f), Sigma: float64(%f), Source: prng}," % (varname, mu, sigma),
    'body': "return uint16(self.%s.Rand())" % (varname)
  }

def genITA(dist, params):
  if dist=='poisson':
    return genExponential("sleepDist", params[0])
  else:
    print('Unknown flow dist %s' % dist)
    return None

# FIXME - Find code for generating a proper Poisson distribution
def genPoisson(varname, param):
  return {
    'decl': "%s dist.Poisson" % (varname),
    'data': "%s: dist.Poisson{Expected: float64(%f), Source: prng}," % (varname, param),
    'body': "var total uint16 = 0\nfor iteration := 0; iteration < int(milliseconds); iteration++ {\n  total=total+uint16(self.%s.Rand())\n}\n\nreturn total" % (varname)
  }

def genContent(dist, params):
  if dist=='multinomial':
    return genMultinomial("contentDist", params)
  else:
    print('Unknown content dist %s' % dist)

def genWeights(varname, params):
  return {
    'decl': "%s []float64" % (varname),
    'data': "%s: []float64 %s," % (varname, genArray(convertToProbs(params)))
  }

def genMultinomial(varname, params):
  return {
    'decl': "%s dist.Multinomial" % (varname),
    'data': "%s: dist.Multinomial{Weights: %sWeights, Source: prng}," % (varname, varname),
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
  return {
    'decl': "coding *huffman.Coding",
    'body': "result.coding, err = huffman.NewCoding([]huffman.BitString %s)\nif err != nil {panic(err)}\n" % (genBitstringArrays(params))
  }

def genEncoder(params):
  varname="contentDist"
  return {
    'decl': "encoder *huffman.Encoder",
    'data': "encoder: huffman.NewEncoder(model.coding),",
    'body': """
      dst := make([]byte, 0, len(bytes))
      self.encoder.Encode(dst, bytes)
      return dst
      """
  }

def genDecoder():
  varname="contentDist"
  return {
    'decl': "decoder *huffman.Decoder",
    'data': "decoder: huffman.NewDecoder(model.coding)",
    'body': """
      dst := make([]byte, 0, len(bytes))
      self.decoder.Decode(dst, bytes)
      return dst
      """
  }

def genArray(arr):
  return '{'+', '.join(map(str, arr))+'}'

def genBitstringArrays(arr):
  return '{'+', '.join(map(genBitstring, arr))+'}'

def genBitstring(arr):
  return "huffman.BitString{Packed: %s, BitLength: %d}" % (genByteArray(packBytes(arr)), len(arr))

def genByteArray(arr):
  return "[]uint8 {" + ', '.join('%#02x' % (byte,) for byte in arr) + '}'

def packBytes(arr):
  bs=[]
  for i in range(len(arr)):
    if i%8==0:
      bs.append(0)
    bs[-1]|=arr[i]<<(7-(i%8))
  return bs

templateName='model.go.airspeed'
testTemplateName='test.go.airspeed'
loader = CachingFileLoader('templates')

def looksLikeModelFile(name):
  return re.search(r'\.(?:yaml|json)\Z', name)

modelDir=sys.argv[1]
packageName=sys.argv[2]
packageDir=sys.argv[3]
packageBase=packageName.split('/')[-1]

if not os.path.exists(packageDir):
  os.mkdir(packageDir)

models=filter(looksLikeModelFile, os.listdir(modelDir))
modelBases=[]
for modelName in models:
  print(modelName)
  modelBasename=modelName.split('.')[0]
  modelBases.append(modelBasename)
  modelFilename=modelDir+'/'+modelName
  outputDir=packageDir+'/'+modelBasename
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

context={'models': modelBases, 'packageName': packageName, 'packageBase': packageBase}
print("Generating models.go")
template = loader.load_template("models.go.airspeed")
body=template.merge(context, loader=loader)
save(body, packageDir+'/'+"models.go")
print("Formatting models.go")
subprocess.call(['gofmt', '-s=true', '-w=true', packageDir+'/'+"models.go"])

#context={'models': modelBases, 'packageName': packageName, 'packageBase': packageBase}
#print("Generating models_test.go")
#template = loader.load_template("models_test.go.airspeed")
#body=template.merge(context, loader=loader)
#save(body, packageDir+'/'+"models_test.go")
#print("Formatting models_test.go")
#subprocess.call(['gofmt', '-s=true', '-w=true', packageDir+'/'+"models_test.go"])
