
"""
  Copyright (c) 2007 Jan-Klaas Kollhof

  This file is part of jsonrpc.

  jsonrpc is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This software is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this software; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
"""

import sys

v3=(sys.version[0]=='3')

try:
  from json import loads, dumps
except ImportError:
  from simplejson import loads, dumps

class JSONRPCException(Exception):
    def __init__(self, rpcError):
        Exception.__init__(self)
        self.error = rpcError

class ServiceProxy(object):
    def __init__(self, channel, addr=None, serviceName=None, methodName=None):
        self.channel = channel
        self.addr=addr
        self.serviceName = serviceName
        self.methodName = methodName

    def __getattr__(self, name):
        if self.methodName != None:
            name = "%s.%s" % (self.methodName, name)
        if self.serviceName:
          if self.addr:
            return ServiceProxy(self.channel, addr=self.addr, serviceName=self.serviceName, methodName=name)
          else:
            return ServiceProxy(self.channel, serviceName=self.serviceName, methodName=name)
        else:
          if self.addr:
            return ServiceProxy(self.channel, addr=self.addr, methodName=name)
          else:
            return ServiceProxy(self.channel, methodName=name)

    def __call__(self, *args):
         if v3:
           postdata = bytes(dumps({"method": self.methodName, 'params': args, 'id':'jsonrpc'}), 'utf-8')
         else:
           postdata = dumps({"method": self.methodName, 'params': args, 'id':'jsonrpc'})
         if self.serviceName:
            if self.addr:
              self.channel.sendto(postdata, self.addr, service=self.serviceName)
            else:
              self.channel.send(postdata, service=self.serviceName)
         else:
            if self.addr:
              self.channel.sendto(postdata, addr)
            else:
              self.channel.send(postdata)
#         respdata = urllib.urlopen(self.channel, postdata).read()
#         resp = loads(respdata)
#         if resp['error'] != None:
#             raise JSONRPCException(resp['error'])
#         else:
#             return resp['result']


