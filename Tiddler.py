import datetime
import os
import sys
from glob import glob
from os.path import exists as pexists
from os.path import join as pjoin
from dateutil.parser import parse as parsedate
from textwrap import dedent


class Tiddler:

    TIME_FORMAT = '%Y%m%d%H%M%S000'

    tiddlers_dir = None

    def save(self):
        if self.tiddlers_dir is None:
            raise Exception('no tiddlers_dir set, refusing to save')
        with open(pjoin(self.tiddlers_dir, self.title + '.tid'), 'w') as ofile:
            ofile.write(unicode(self))

    def _parse(self, raw_tiddler):
        '''
        :type raw_tiddler: str
        :return: None
        '''
        DIVIDER = '\n\n'
        idx_header_end = raw_tiddler.index(DIVIDER)
        header_lines = raw_tiddler[:idx_header_end]
        to_parse = {
            'created': False,
            'modified': False,
            'tags': False,
            'title': False,
            'type': False,
        }
        for line_ in header_lines:
            line = line_.strip()
            for field_name in to_parse:
                entry_marker = '%s:'%field_name
                if not line.startswith(entry_marker):
                    continue
                data = line.split(None, 1)[-1]
                if entry_marker in ('created', 'modified'):
                    setattr(self, entry_marker, parsedate(data))
                elif entry_marker == 'tags':
                    self.tags = [tag.strip() for tag in data.split(',')]
                elif entry_marker == 'title':
                    self.title = data
                elif entry_marker == 'type':
                    self.type = data
        self.content = raw_tiddler[idx_header_end+2:].decode('utf-8')

    def read_string(self, s):
        self._parse(s)

    def read(self, file_path):
        with open(file_path) as ifile:
            raw_tiddler = ifile.read()
            self.read_string(raw_tiddler)

    @classmethod
    def load_from_file(cls, file_path):
        t = cls(None)
        t.read(file_path)
        return t

    def __init__(self, title, content=None, content_type=None):
        self.title = title
        self.content = content or ''
        self.creator = None
        self.modifier = None
        self.created = datetime.datetime.now()
        self.modified = self.created
        self.links = []
        self.linksUpdated = False
        self.tags = []
        self.fields = {}
        self.type = content_type or 'text/vnd.tiddlywiki'

    def add_tag(self, tag):
        if ' ' in tag:
            self.tags.append('[[%s]]' % tag)
        else:
            self.tags.append(tag)

    def __unicode__(self):
        return dedent(u'''\
            created: %s
            modified: %s
            tags: %s
            title: %s
            type: %s

            %s''') % (
                self.created.strftime(self.TIME_FORMAT),
                self.modified.strftime(self.TIME_FORMAT),
                ' '.join(self.tags),
                self.title,
                self.type,
                self.content,
            )
