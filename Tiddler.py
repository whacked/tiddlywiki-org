import sys
import os
from os.path import exists as pexists, join as pjoin
from glob import glob
import datetime
import email_parser as emparse
reload(emparse)

class Tiddler:

    tiddlers_dir = None

    def save(self):
        if self.tiddlers_dir is None:
            raise Exception('no tiddlers_dir set, refusing to save')
        with open(pjoin(self.tiddlers_dir, self.title + '.tid'), 'w') as ofile:
            ofile.write(unicode(self))

    def __init__(self, title, ):
        self.title = title
        self.text = ''
        self.creator = None
        self.modifier = None
        self.created = datetime.datetime.now()
        self.modified = self.created
        self.links = []
        self.linksUpdated = False
        self.tags = []
        self.fields = {}
        self.type = 'text/vnd.tiddlywiki'

    def add_tag(self, tag):
        if ' ' in tag:
            self.tags.append('[[%s]]' % tag)
        else:
            self.tags.append(tag)

    def __unicode__(self):
        return '''\
created: %s
modified: %s
tags: %s
title: %s
type: %s

%s''' % (
    self.created.strftime('%Y%m%d%H%M%S000'),
    self.modified.strftime('%Y%m%d%H%M%S000'),
    ' '.join(self.tags),
    self.title,
    self.type,
    self.text,
)
    
if __name__ == '__main__':
    Tiddler.tiddlers_dir = 'tw/tiddlers'
    tid = Tiddler('hello world again')
    # print unicode(tid)
    # tid.save()

    from termcolor import colored
    import mailbox
    import html2text as h2t
    from jwzthreading import thread, make_message

    def to_text_list(msg):
        rtn = []
        if msg.is_multipart():
            for part in msg.get_payload():
                rtn.extend(to_text_list(part))
                break
        else:
            rtn.append(msg.get_payload())
        return rtn

    class Store:
        count = 0

    org_out = []
    SHOULD_OUTPUT_TIDDLER = False
    def print_container(ctr, depth=0, parent=None):
        import sys
        sys.stdout.write(depth*' ')
        if ctr.message:
            sys.stdout.write(colored('%s -- %s' % (
                ctr.message.subject,
                ctr.message.message.get('from'),
            ), 'yellow'))

            org_out.append('%s %s %s' % (
                '*'*(1+depth),
                ctr.message.subject,
                ctr.message.message.get('from'),
            ))

            tiddler_name = '%s (%s)' % (
                ctr.message.subject.replace(':', '_'),
                Store.count,
            )
            Store.count += 1

            if SHOULD_OUTPUT_TIDDLER:
                tid = Tiddler(tiddler_name)
                if parent:
                    tid.text = 'in response to: {{%s}}\n\n' % (parent)
                else:
                    tid.text = ''
                tid.text += '\n'.join(to_text_list(ctr.message.message))
                # tid.type = 'text/x-markdown'
                tid.save()
                print '\n>>> saved: %s' % tid.title

            # orgmode text output
            if True:
                print
                for n, ltext in enumerate(emparse.Parser.parse('\n'.join(to_text_list(ctr.message.message))), start=1):
                    # print ltext
                    if ltext.depth == 0:
                        org_out.append(ltext.text)
                    elif True:
                        pass
                    else:
                        org_out.append('''\
#+BEGIN_EXAMPLE
%s
#+END_EXAMPLE
''' % ltext.text)
                print n, 'lines processed.'


            sys.stdout.write('\n')
            # for text in to_text_list(ctr.message.message):
            #     # print h2t.html2text(text.replace('\r\n', '\n').replace('=\n', ''))
            #     print text
        # if raw_input().strip() == 'q': return
        for c in ctr.children:
            print_container(c, depth+1, tiddler_name)

    print('Reading input file...')

    if False:
        mbox_path = '/Volumes/ramdisk/ccmt6.mbox'
    # with open(mbox_path, 'rb') as ifile:
        # mbox = mailbox.UnixMailbox(ifile)
        mbox = mailbox.mbox(mbox_path)
        mlist = list(mbox)
        msglist = [make_message(m) for m in mlist]
    else:
        eml_path = '/tmp/ccmt-1'
        mlist = [mailbox.mboxMessage(open(eml).read()) for eml in glob(pjoin(eml_path, '*.eml'))]
        msglist = [make_message(m) for m in mlist]

    print('Threading...')
    subject_table = thread(msglist)

    # Output
    L = subject_table.items()
    L.sort()
    for subj, container in L:
        print_container(container)

    with open('ccmt-1.org', 'w') as ofile:
        ofile.write('\n'.join(org_out))
        print 'ok:',ofile.name
