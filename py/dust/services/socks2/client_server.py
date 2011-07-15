import sys
import time

import monocle
from monocle import _o
monocle.init(sys.argv[1])

from monocle.stack import eventloop
from monocle.stack.network import add_service, Service, Client, ConnectionLost

@_o
def handle_echo(conn):
    while True:
        try:
            message = yield conn.read_until('\r\n')
        except ConnectionLost:
            break
        yield conn.write("you said: %s\r\n" % message.strip())

@_o
def do_echos():
    try:
        client = Client()
        yield client.connect('localhost', 8000)
        t = time.time()
        for x in xrange(10000):
            msg = "hello, world #%s!" % x
            yield client.write(msg + '\r\n')
            echo_result = yield client.read_until("\r\n")
            assert echo_result.strip() == "you said: %s" % msg
        print '10000 loops in %.2fs' % (time.time() - t)
    finally:
        client.close()
        eventloop.halt()

add_service(Service(handle_echo, port=8000))
monocle.launch(do_echos)
eventloop.run()
