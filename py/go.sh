python3.0 commands/make-invite.py test 7000 6
rm config/outgoing_invites.ip
python3.0 commands/process-invite.py test test
python3.0 commands/serve.py 7000 6 test