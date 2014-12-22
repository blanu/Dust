import os
import yaml
import subprocess
from airspeed import CachingFileLoader

def parse(filename):
  f=open(filename)
  data=f.read()
  f.close()

  obj=yaml.load(data)
  return obj

def save(data, filename):
  f=open(filename, 'w')
  f.write(data)
  f.close()

def convertModel(data):
  model={}
  model['name']=data['name']
  model['duration']=genDuration(data['duration']['dist'], data['duration']['params'])
  model['packet_length']=genLength(data['length']['dist'], data['length']['params'])
  model['packet_count']=genFlow(data['flow']['dist'], data['flow']['params'])
  model['encode']={'body': 'return bytes', 'data': 'var contentDist=[]float64 {0.5,0.5}'}
  model['decode']={'body': 'return bytes'}
  model['add_padding']={'body': 'return bytes', 'data': 'var paddingDist=Multinomial(contentDist)'}
  model['strip_padding']={'body': 'return bytes'}
  model['random_bytes']={'body': 'return nil'}

  return model

def genDuration(dist, params):
  if dist=='exponential':
    return genExponential("durationDist", params[0])
  else:
    print('Unknown duration dist %s' % dist)
    return None

def genExponential(varname, param):
  return {
    'data': "var %s=Exponential(%d)" % (varname, param),
    'body': "var d=&dist.Exponential{Rate: float64(%s)}\nreturn uint16(d.Rand())" % (varname)
  }

def genLength(dist, params):
  if dist=='normal':
    return genNormal("lengthDist", params[0], params[1])
  else:
    print('Unknown length dist %s' % dist)
    return None

def genNormal(varname, mu, sigma):
  return {
    'data': "var %s=&Normal{Mean:%f, Sd:%f}" % (varname, mu, sigma),
    'body': "var d=dist.Normal{Mu: float64(%s.Mean), Sigma: float64(%s.Sd)}\nreturn uint16(d.Rand())" % (varname, varname)
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
    'data': "var %s=&Normal{Mean:%f, Sd:%f}" % (varname, param, param),
    'body': "var d=dist.Normal{Mu: float64(%s.Mean), Sigma: float64(%s.Sd)}\nreturn uint16(d.Rand())" % (varname, varname)
  }

templateName='model.go.airspeed'
testTemplateName='test.go.airspeed'

models=os.listdir('models')
for modelName in models:
  print(modelName)
  modelName=modelName.split('.')[0]
  yamlName='models/'+modelName+'.yaml'
  outputName=modelName+'.go'
  testOutputName=modelName+'_test.go'

  loader = CachingFileLoader('templates')

  model=parse(yamlName)
  context=convertModel(model)

  print("Generating %s" % (modelName+'.go'))
  template = loader.load_template(templateName)
  body=template.merge(context, loader=loader)
  save(body, outputName)
  print("Formatting %s" % (modelName+'.go'))
  subprocess.call(['gofmt', '-s=true', '-w=true', outputName])

  print("Generating %s" % (modelName+'_test.go'))
  template = loader.load_template(testTemplateName)
  body=template.merge(context, loader=loader)
  save(body, testOutputName)
  print("Formatting %s" % (modelName+'_test.go'))
  subprocess.call(['gofmt', '-s=true', '-w=true', testOutputName])
