from mail.mailService import MailHandler
from chat.chatService import ChatHandler
from file.fileService import FileHandler

mh=MailHandler()
ch=ChatHandler()
fh=FileHandler()

services={
  'mail': mh.handle,
  'chat': ch.handle,
  'file': fh.handle,
}