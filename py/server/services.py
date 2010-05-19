from mail.mailService import MailHandler
from file.fileService import FileHandler

mh=MailHandler()
fh=FileHandler()

services={
  'mail': mh.handle,
  'file': fh.handle,
}