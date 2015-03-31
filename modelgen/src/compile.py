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
  model['packet_length']=genLength(data['incomingModel']['length']['dist'], data['incomingModel']['length']['params'], data['outgoingModel']['length']['params'], lang)
  model['packet_sleep']=genIAT(data['incomingModel']['flow']['dist'], data['incomingModel']['flow']['params'], data['outgoingModel']['flow']['params'], lang)
  model['huffman']=genHuffman(data['incomingModel']['huffman'], data['outgoingModel']['huffman'])
  model['sequence']=genSequence('sequence', data['incomingModel']['sequence'], data['outgoingModel']['sequence'])
  model['encode']=genEncoder()
  model['decode']=genDecoder()

  return model

def genSequence(varname, data, data2):
  return {
    'decl': "encoding_%s []byte\ndecoding_%s []byte\nencoding_position uint64\ndecoding_position uint64" % (varname),
    'incoming': "encoding_%s: []byte{%s}" % (varname, makeByteArray(data)),
    'outgoing': "decoding_%s: []byte{%s}" % (varname, makeByteArray(data2)),
  }

def makeByteArray(data):
  return ", ".join(map(str, data))

def genDuration(dist, params, lang):
  if dist=='exponential':
    return genExponential("durationDist", params[0], lang)
  else:
    print('Unknown duration dist %s' % dist)
    return None

def genExponential(varname, param1, param2, lang):
  if lang=='go':
    return {
      'decl': "%s dist.Exponential" % (varname),
      'incoming': "%s: dist.Exponential{Rate: float64(%s), Source: prng}," % (varname, param1),
      'outgoing': "%s: dist.Exponential{Rate: float64(%s), Source: prng}," % (varname, param2),
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

def genLength(dist, params1, params2, lang):
  if dist=='normal':
    return genNormal("lengthDist", params1[0], params1[1], params2[0], params2[1], lang)
  elif dist=='multinomial':
    return genMultinomial("lengthDist", params1, params2, lang)
  else:
    print('Unknown length dist %s' % dist)
    return None

def genNormal(varname, mu1, sigma1, mu2, sigma2, lang):
  if lang=='go':
    return {
      'decl': "%s dist.Normal" % (varname),
      'incoming': "%s: dist.Normal{Mu: float64(%f), Sigma: float64(%f), Source: prng}," % (varname, mu1, sigma1),
      'outgoing': "%s: dist.Normal{Mu: float64(%f), Sigma: float64(%f), Source: prng}," % (varname, mu2, sigma2),
      'expr': "clampUint16(self.%s.Rand())" % (varname)
    }
  elif lang=='js':
    return {
      'data': "this.%s=[%f, %f]" % (varname, mu, sigma),
      'expr': "boundedIntegerRandomNormal(this.%s[0], this.%s[1], 1, this.MaxPacketLength())" % (varname, varname)
    }
  else:
    return {}

def genIAT(dist, params1, params2, lang):
  if dist=='poisson':
    return genExponential("sleepDist", params1[0], params2[0], lang)
  else:
    print('Unknown flow dist %s' % dist)
    return None

def genHuffman(params1, params2):
  return {
    'decl': "incoming_coding *huffman.Coding\noutgoing_coding *huffman.Coding",
    'incoming': "result.incoming_coding, err = huffman.NewCoding([]huffman.BitString %s)\nif err != nil {panic(err)}\n" % (genBitstringArrays(params1)),
    'outgoing': "result.outgoing_coding, err = huffman.NewCoding([]huffman.BitString %s)\nif err != nil {panic(err)}\n" % (genBitstringArrays(params2))
  }

def genEncoder():
  varname="contentDist"
  return {
    'decl': "encoder *huffman.Encoder",
    'incoming': "encoder: huffman.NewEncoder(model.incoming_coding),",
    'outgoing': "encoder: huffman.NewEncoder(model.outgoing_coding),",
    'body': """
      return codec.encoder.Encode(dst, src)
      """
  }

def genDecoder():
  varname="contentDist"
  return {
    'decl': "decoder *huffman.Decoder",
    'incoming': "decoder: huffman.NewDecoder(model.incoming_coding)",
    'outgoing': "decoder: huffman.NewDecoder(model.outgoing_coding)",
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
