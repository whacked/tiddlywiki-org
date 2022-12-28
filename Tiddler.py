import datetime
import os
import os.path as _p
import re
import sys
from dateutil.parser import parse as parsedate
from glob import glob
from textwrap import dedent


class Tiddler:

    TIME_FORMAT = '%Y%m%d%H%M%S000'

    tiddlers_dir = None

    def save(self):
        if self.tiddlers_dir is None:
            raise Exception('no tiddlers_dir set, refusing to save')
        with open(_p.join(self.tiddlers_dir, self.title + '.tid'), 'w') as ofile:
            ofile.write(unicode(self))

    @classmethod
    def get_metadata_file_path(cls, tiddler_file_path):
        return f'{tiddler_file_path}.meta'

    @classmethod
    def parse_metadata_string(cls, metadata_string):
        metadata = {}
        for line_ in metadata_string.splitlines():
            line = line_.strip()
            maybe_match = re.match(r'([^:]+):\s*(.+)', line.strip())
            if maybe_match is None:
                continue
            else:
                field_name, value = maybe_match.groups()
            metadata[field_name] = value
        return metadata

    @classmethod
    def _decode_tags(cls, tag_string):
        tags = []
        def _recur(remain):
            next_index = None
            if remain.startswith('[['):
                next_index = remain.index(']]')
                tag = remain[2:next_index]
                next_index += 3
            else:
                try:
                    next_index = remain.index(' ')
                    tag = remain[:next_index]
                    next_index += 1
                except:
                    tag = remain
            if tag:
                tags.append(tag)
            if next_index is not None:
                _recur(remain[next_index:])
        _recur(tag_string)
        return tags

    @classmethod
    def _encode_tags(cls, tags):
        out = []
        for tag in tags:
            if ' ' in tag:
                out.append(f'[[{tag}]]')
            else:
                out.append(tag)
        return ' '.join(out)

    def _parse(self, raw_tiddler):
        '''
        :type raw_tiddler: str
        :return: None
        '''
        DIVIDER = '\n\n'
        try:
            idx_header_end = raw_tiddler.index(DIVIDER)
            header_text = raw_tiddler[:idx_header_end]
            self.content = raw_tiddler[idx_header_end+2:]
        except:
            idx_header_end = None
            header_text = raw_tiddler
            self.content = None
        self.metadata = self.__class__.parse_metadata_string(header_text)
        for field_name, value in self.metadata.items():
            if field_name in ('created', 'modified'):
                setattr(self, field_name, parsedate(
                    f'{value[:4]}-{value[4:6]}-{value[6:8]} '
                    f'{value[8:10]}:{value[10:12]}:{value[12:14]}.{value[14:]}'
                ))
            elif field_name == 'tags':
                self.tags = self.__class__._decode_tags(value)
            elif field_name == 'title':
                self.title = value
            elif field_name == 'type':
                self.type = value

    def read_string(self, s):
        self._parse(s)

    def read(self, file_path):
        with open(file_path, 'rb') as ifile:
            raw_tiddler = ifile.read().decode('utf-8')
            self.read_string(raw_tiddler)

    @classmethod
    def load_from_file(cls, file_path):
        if file_path.endswith('.tid'):
            t = cls(None)
            t.read(file_path)
        else:
            metadata_file_path = cls.get_metadata_file_path(file_path)
            with open(file_path, 'rb') as ifile:
                content = ifile.read()
            with open(metadata_file_path) as ifile:
                metadata = cls.parse_metadata_string(ifile.read())
                t = cls(
                    metadata['title'],
                    content,
                    metadata['type']
                )
                t.set_metadata(**metadata)
        return t

    def set_metadata(self, **kwargs):
        for k, v in kwargs.items():
            self.metadata[k] = v

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
        self.metadata = {}
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

def load_tiddlymap_positions(tiddlywiki_directory):
    import json
    tiddlywiki_base_directory = None
    for candidate_directory in [
        tiddlywiki_directory,
        _p.join(tiddlywiki_directory, '..'),
    ]:
        if _p.exists(_p.join(candidate_directory, 'tiddlywiki.info')):
            tiddlywiki_base_directory = candidate_directory
            break
    tiddlymap_positions_file_path = _p.join(
        tiddlywiki_base_directory,
        'tiddlers',
        '$__plugins_felixhayashi_tiddlymap_graph_views_all_map.tid'
    )
    tiddlymap_positions_tiddler = Tiddler.load_from_file(
        tiddlymap_positions_file_path)
    positions = json.loads(tiddlymap_positions_tiddler.content)
    return positions
