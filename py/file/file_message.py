import json
import time
import struct
import random

from core.dust_packet import makeLength

class FileMessage:
  def __init__(self):
    self.headersLength=None
    self.headersLengthValue=None
    self.headers=None
    self.data=None
    self.message=None

  def createFileMessage(self, headers, data):
    self.headers=json.write(headers)
    self.headersLengthValue=len(self.headers)
    self.headersLength=makeLength(self.headersLengthValue)
    self.data=data
    if self.data:
      self.message=self.headersLength+self.headers+self.data
    else:
      self.message=self.headersLength+self.headers

  def decodeFileMessage(self, message):
    self.message=message
    self.headersLength=self.message[:4]
    self.headersLengthValue=struct.unpack("I", self.offset)[0]
    self.headers=json.read(self.message[4:self.headersLengthValue+4])
    if self.headersLengthValue+4==len(self.data):
      self.data=None
    else:
      self.data=self.message[self.headersLengthValue+4:]
