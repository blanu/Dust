python3.1 commands/make-invite.py test 7040 6
rm config/outgoing_invites.ip
python3.1 commands/process-invite.py test test
python3.1 commands/tracker-serve.py 7040 6 test