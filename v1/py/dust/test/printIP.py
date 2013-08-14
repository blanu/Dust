import sys

from dust.core.util import getPublicIP

ipv=sys.argv[1]

if ipv=='6':
  print(getPublicIP(True))
elif ipv=='4':
  print(getPublicIP(False))
else:
  print('Unknown ip version:', ipv)