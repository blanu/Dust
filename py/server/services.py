from mail.mailService import MailHandler

mh=MailHandler()

services={
  'mail': mh.handle,
}