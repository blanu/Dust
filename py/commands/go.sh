python3.1 commands/make-invite.py test 7000 6
#rm config/outgoing_invites.ip
python3.1 commands/process-invite.py test test
python3.1 commands/serve.py 7000 6 test [2001:0:53aa:64c:186e:3ef6:b8eb:987d]:7040