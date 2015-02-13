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

def convertModel(name, data, lang):
  model={}
  model['packagename']=name
  model['name']=name
  model['model_type']=name+'Model'
  model['codec_type']=name+'Codec'
  model['duration']=genDuration(data['duration']['dist'], data['duration']['params'], lang)
  model['packet_length']=genLength(data['incomingModel']['length']['dist'], data['incomingModel']['length']['params'])
  model['packet_sleep']=genITA(data['incomingModel']['flow']['dist'], data['incomingModel']['flow']['params'], lang)
  model['huffman']=genHuffman(data['incomingModel']['huffman'])
  model['encode']=genEncoder()
  model['decode']=genDecoder()

  return model

def genDuration(dist, params, lang):
  if dist=='exponential':
    return genExponential("durationDist", params[0], lang)
  else:
    print('Unknown duration dist %s' % dist)
    return None

def genExponential(varname, param, lang):
  if lang=='go':
    return {
      'decl': "%s dist.Exponential" % (varname),
      'data': "%s: dist.Exponential{Rate: float64(%s), Source: prng}," % (varname, param),
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
    'expr': "clampUint16(self.%s.Rand())" % (varname)
  }

def genITA(dist, params, lang):
  if dist=='poisson':
    return genExponential("sleepDist", params[0], lang)
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

def genHuffman(params):
  return {
    'decl': "coding *huffman.Coding",
    'body': "result.coding, err = huffman.NewCoding([]huffman.BitString %s)\nif err != nil {panic(err)}\n" % (genBitstringArrays(params))
  }

def genEncoder():
  varname="contentDist"
  return {
    'decl': "encoder *huffman.Encoder",
    'data': "encoder: huffman.NewEncoder(model.coding),",
    'body': """
      return codec.encoder.Encode(dst, src)
      """
  }

def genDecoder():
  varname="contentDist"
  return {
    'decl': "decoder *huffman.Decoder",
    'data': "decoder: huffman.NewDecoder(model.coding)",
    'body': """
      return codec.decoder.Decode(dst, src)
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
