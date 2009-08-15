from mail.mailService import MailHandler
from chat.chatService import ChatHandler

mh=MailHandler()
ch=ChatHandler()

services={
  'mail': mh.handle,
  'chat': ch.handle,
}