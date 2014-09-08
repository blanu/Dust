import sys

import monocle
from monocle import _o
monocle.init(sys.argv[1])

from monocle.stack import eventloop
from monocle.stack.network import add_service, Service

@_o
def echo(conn):
    their_message = yield conn.readline()
    yield conn.write("you said: %s\r\n" % their_message.strip())

add_service(Service(echo, 7050))
eventloop.run()
