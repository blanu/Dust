#from mail.mailService import MailHandler
#from chat.chatService import ChatHandler
#from file.fileService import FileHandler
from services.tracker.trackerService import TrackerService
from services.tracker.trackbackService import TrackbackService
from services.dustmail.dustmailService import DustmailService

#mh=MailHandler()
#ch=ChatHandler()
#fh=FileHandler()
tracker=TrackerService()
trackback=TrackbackService()
dustmail=DustmailService()

services={
#  'mail': mh,
#  'chat': ch,
#  'file': fh,
  'tracker': tracker,
  'trackback': trackback,
  'dustmail': dustmail,
}