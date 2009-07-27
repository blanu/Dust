import invite

myip=invite.loadInvitePackage('config/myinvites.yaml')
ip=invite.loadInvitePackage('config/invites.ip')
myip.merge(ip)
myip.save('config/myinvites.yaml')