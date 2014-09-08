import yaml
import email
import smtplib

from email.mime.text import MIMEText

class Notifier:
  def __init__(self, frm):
    self.frm=frm

    f=open('config/emailServer.yaml', 'r')
    self.config=yaml.load(f.read())
    f.close()

  def notify(self, to, subject, body):
    print('Notifying '+str(to)+'...')
    msg=MIMEText(body)
    msg['From']=self.frm
    msg['To']=to
    msg['Subject']=subject

    smtp = smtplib.SMTP(self.config['smtpHost'])
    smtp.set_debuglevel(1)
    smtp.sendmail(self.frm, to, msg.as_string())
    smtp.quit()

if __name__=='__main__':
  notifier=Notifier('brandon@blanu.net')
  notifier.notify('brandon@blanu.net', 'Test Message', 'This is a test of the DustMail notifier system.')
