import sys

from invite.invite import loadInvitePackage

passwd=sys.argv[1]
mypasswd=sys.argv[2]

myip=loadInvitePackage('config/myinvites.ip', mypasswd)
ip=loadInvitePackage('config/invites.ip', passwd)
myip.merge(ip)
myip.save('config/myinvites.ip', mypasswd)