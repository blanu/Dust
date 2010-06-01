#from mail.mailService import MailHandler
#from chat.chatService import ChatHandler
#from dust.services.file.fileService import FileHandler
from dust.services.tracker.trackerService import TrackerService
from dust.services.tracker.trackbackService import TrackbackService
from dust.services.dustmail.dustmailService import DustmailService

#mh=MailHandler()
#ch=ChatHandler()
#fh=FileHandler()
tracker=TrackerService()
trackback=TrackbackService()
dustmail=DustmailService()

activeServices={
#  'mail': mh,
#  'chat': ch,
#  'file': fh,
  'tracker': tracker,
  'trackback': trackback,
  'dustmail': dustmail,
}