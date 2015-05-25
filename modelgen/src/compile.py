import os
import sys
import yaml
import json
import subprocess
import re
from airspeed import CachingFileLoader
from numpy import mean
from numpy.random import multinomial
import math

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

def convertModel(name, data, lang):
  model={}
  model['packagename']=name
  model['name']=name
  model['model_type']=name+'Model'
  model['codec_type']=name+'Codec'
  if 'length' in data['incomingModel']:
    model['packet_length']=genLength(data['incomingModel']['length']['dist'], data['incomingModel']['length']['params'], data['outgoingModel']['length']['params'], lang)
  else:
    model['packet_length']=genEmptyLength()
  if 'flow' in data['incomingModel']:
    avgLength1=averageLength(data['incomingModel']['length']['params'])
    avgLength2=averageLength(data['outgoingModel']['length']['params'])
    model['packet_sleep']=genIAT(data['incomingModel']['flow']['dist'], data['incomingModel']['flow']['params'], data['outgoingModel']['flow']['params'], avgLength1, avgLength2, lang)
  else:
    model['packet_sleep']=genEmptyIAT()
  model['huffman']=genHuffman(data['incomingModel']['huffman'], data['outgoingModel']['huffman'])
  model['sequence']=genSequence(data['incomingModel']['sequence'], data['outgoingModel']['sequence'])
  model['encode']=genEncoder()
  model['decode']=genDecoder()
  model['max_sleep']='enc1.MaxSleep = 60000 * time.Millisecond' # 1 minute

  return model

def averageLength(probs):
  samples=multinomial(100000, probs)
  return mean(samples)

def genSequence(data, data2):
  return {
    'incoming': "Prefix: []byte{%s}" % (makeByteArray(data)),
    'outgoing': "Prefix: []byte{%s}" % (makeByteArray(data2)),
  }

def makeByteArray(data):
  return ", ".join(map(str, data))

def genDuration(dist, params, lang):
  if dist=='exponential':
    return genExponential("durationDist", params[0], lang)
  else:
    print('Unknown duration dist %s' % dist)
    return None

def genExponential(varname, param1, param2, avgLength1, avgLength2, lang):
  param1=clampRate(param1, avgLength1)
  param2=clampRate(param2, avgLength2)
  if lang=='go':
    return {
      'incoming': "enc1.%s = dist.Exponential{Rate: float64(%s), Source: model.prng}" % (varname, param1),
      'outgoing': "enc1.%s = dist.Exponential{Rate: float64(%s), Source: model.prng}" % (varname, param2),
      'expr': "clampUint16(self.%s.Rand())" % (varname)
    }
  elif lang=='js':
    return {
      'decl': "this.%s=null;" % (varname),
      'data': "this.%s=%s;" % (varname, param),
      'expr': "clampUint16(randomExponential(this.%s))" % (varname)
    }
  else:
    return {}

def clampRate(param, avgLength):
  print("Poisson params: %f %f" % (param, avgLength))
  bpms=float(param*avgLength) # bytes per millisecond
  kbps=(bpms*8)/1000 # kilobits per second
  print("%f kpbs" % (kbps))
  if kbps < 56: # 56kbps minimum
    ratio=math.ceil(56/kbps)
    print("Raising %f" % (ratio))
    ratio=ratio*1.5
    print('Increading 50%')
    param=param*ratio
  elif kbps > 560: # 560kbps maximum
    ratio=math.floor(kbps/560)
    print("Lowering %f" % (ratio))
    param=param/ratio
  print("Final Poisson: %f" % (param))
#  return param
  return 0.5

def genLength(dist, params1, params2, lang):
  if dist=='normal':
    return genNormal("LengthDist", params1[0], params1[1], params2[0], params2[1], lang)
  elif dist=='multinomial':
    return genMultinomial("LengthDist", params1, params2, lang)
  else:
    print('Unknown length dist %s' % dist)
    return None

def genEmptyLength():
  if lang=='go':
    return {
      'incoming': "",
      'outgoing': "",
      'expr': "1440"
    }
  else:
    return {}

def genNormal(varname, mu1, sigma1, mu2, sigma2, lang):
  if lang=='go':
    return {
      'incoming': "enc1.%s = dist.Normal{Mu: float64(%f), Sigma: float64(%f), Source: model.prng}" % (varname, mu1, sigma1),
      'outgoing': "enc1.%s = dist.Normal{Mu: float64(%f), Sigma: float64(%f), Source: model.prng}" % (varname, mu2, sigma2),
      'expr': "clampUint16(self.%s.Rand())" % (varname)
    }
  elif lang=='js':
    return {
      'data': "this.%s=[%f, %f]" % (varname, mu, sigma),
      'expr': "boundedIntegerRandomNormal(this.%s[0], this.%s[1], 1, this.MaxPacketLength())" % (varname, varname)
    }
  else:
    return {}

def genMultinomial(varname, params1, params2, lang):
  if lang=='go':
    return {
      'incoming': "enc1.%s = dist.Multinomial{Weights: []float64 %s, Source: model.prng}" % (varname, genArray(params1)),
      'outgoing': "enc1.%s = dist.Multinomial{Weights: []float64 %s, Source: model.prng}" % (varname, genArray(params2)),
      'expr': "clampUint16(self.%s.Rand())" % (varname)
    }
  else:
    return {}

def genIAT(dist, params1, params2, avgLength1, avgLength2, lang):
  if dist=='poisson':
    return genExponential("SleepDist", params1[0], params2[0], avgLength1, avgLength2, lang)
  else:
    print('Unknown flow dist %s' % dist)
    return None

def genEmptyIAT():
  if lang=='go':
    return {
      'incoming': "",
      'outgoing': "",
      'expr': "20"
    }
  else:
    return {}

def genHuffman(params1, params2):
  return {
    'incoming': 'HuffTable: []huffman.BitString' + genBitstringArrays(params1),
    'outgoing': 'HuffTable: []huffman.BitString' + genBitstringArrays(params2)
  }

def genEncoder():
  varname="contentDist"
  return {
    'incoming': "encoder: huffman.NewEncoder(model.incoming_coding),",
    'outgoing': "encoder: huffman.NewEncoder(model.outgoing_coding),"
  }

def genDecoder():
  varname="contentDist"
  return {
    'incoming': "decoder: huffman.NewDecoder(model.incoming_coding)",
    'outgoing': "decoder: huffman.NewDecoder(model.outgoing_coding)"
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

def looksLikeModelFile(name):
  return re.search(r'\.(?:yaml|json)\Z', name)

lang=sys.argv[1]
modelDir=sys.argv[2]
packageName=sys.argv[3]
packageDir=sys.argv[4]
packageBase=packageName.split('/')[-1]

templateName='model.%s.airspeed' % (lang)
testTemplateName='test.%s.airspeed' % (lang)
loader = CachingFileLoader('templates')

if not os.path.exists(packageDir):
  os.mkdir(packageDir)

def formatCode(filename, lang):
  if lang=='go':
    print("Formatting %s" % (filename))
    subprocess.call(['gofmt', '-s=true', '-w=true', filename])
  elif lang=='js':
    print("Formatting %s" % (filename))
    subprocess.call(['jshint', '--config', 'lib/jshintrc', filename])
  else:
    print('No formatter for %s...' % (lang))

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
  outputName=outputDir+'/'+modelBasename+'.'+lang
  testOutputName=outputDir+'/'+modelBasename+'_test.'+lang

  model=parse(modelFilename)
  context=convertModel(modelBasename, model, lang)

  print("Generating %s" % (modelBasename+'.'+lang))
  template = loader.load_template(templateName)
  body=template.merge(context, loader=loader)
  save(body, outputName)
  print("Formatting %s" % (modelBasename+'.'+lang))
  formatCode(outputName, lang)

  print("Generating test %s" % (modelBasename+'_test.'+lang))
  template = loader.load_template(testTemplateName)
  body=template.merge(context, loader=loader)
  save(body, testOutputName)
  print("Formatting test %s" % (modelBasename+'_test.'+lang))
  formatCode(testOutputName, lang)

if lang=='go':
  context={'models': modelBases, 'packageName': packageName, 'packageBase': packageBase}
  print("Generating models.go")
  template = loader.load_template("models.%s.airspeed" % (lang))
  body=template.merge(context, loader=loader)
  save(body, packageDir+'/'+"models."+lang)
  print("Formatting models."+lang)
  formatCode(packageDir+'/'+"models.go", lang)

#context={'models': modelBases, 'packageName': packageName, 'packageBase': packageBase}
#print("Generating models_test.go")
#template = loader.load_template("models_test.go.airspeed")
#body=template.merge(context, loader=loader)
#save(body, packageDir+'/'+"models_test.go")
#print("Formatting models_test.go")
#subprocess.call(['gofmt', '-s=true', '-w=true', packageDir+'/'+"models_test.go"])
