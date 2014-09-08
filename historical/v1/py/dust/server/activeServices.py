from dust.util.ymap import YamlMap

activeServices={}

paths=YamlMap('config/activeServices.yaml')
for serviceName in paths.keys():
  modName, clsName=paths[serviceName]
  path=modName.split('.')
  mod=__import__(modName)
  for name in path[1:]:
    mod=getattr(mod, name)
  cls=getattr(mod, clsName)
  obj=cls()
  activeServices[serviceName]=obj
