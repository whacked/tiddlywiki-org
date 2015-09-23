import sys
import os.path as p
import datetime
import json
import urllib2

ORG_MODE_PARSER_BASEPATH = 'https://raw.githubusercontent.com/whacked/org-mode-parser/client-side-rendering'
# ORG_MODE_PARSER_BASEPATH = p.expanduser('~/PATH/TO/local/fork')
UNDERSCORE_JS_PATH = 'https://raw.githubusercontent.com/jashkenas/underscore/master/underscore-min.js'
# UNDERSCORE_JS_PATH = p.join(ORG_MODE_PARSER_BASEPATH, 'lib/underscore-min.js')

def get_content(path):
    if path.startswith('http'):
        print 'getting path', path
        res = urllib2.urlopen(path)
        return res.read()
    else:
        # assume local file
        if not p.exists(path):
            raise IOError('Could not locate file: %s'%path)
        else:
            return open(path).read()
    raise ValueError('failed to get content for given path: %s'%path)

out = {
    "tiddlers": {
        "$:/language/Docs/Types/text/org": {
            "title": "$:/language/Docs/Types/text/org",
            "description": "org mode",
            "name": "text/org"
        },
        # the actual parser library
        "$:/plugins/tiddlywiki/org-mode-parser/org-mode-parser.js": {
            "type": "application/javascript",
            "title": "$:/plugins/tiddlywiki/org-mode-parser/org-mode-parser.js",
            "module-type": "library",
        },
        # underscore lib
        "$:/plugins/tiddlywiki/org-mode-parser/underscore.js": {
            "type": "application/javascript",
            "title": "$:/plugins/tiddlywiki/org-mode-parser/underscore.js",
            "module-type": "library",
        },
        # interaction/glue code for TW
        "$:/plugins/tiddlywiki/org-mode-parser/wrapper.js": {
            "title": "$:/plugins/tiddlywiki/org-mode-parser/wrapper.js",
            "type": "application/javascript",
            "module-type": "parser"
        }
    }
}


plugin_tohtml = get_content(p.join(ORG_MODE_PARSER_BASEPATH, 'lib/plugin-tohtml.js'))

tiddlers = out["tiddlers"]
BASE_NAMESPACE = "$:/plugins/tiddlywiki/org-mode-parser"
tiddlers[BASE_NAMESPACE+"/org-mode-parser.js"]["text"] = get_content(p.join(ORG_MODE_PARSER_BASEPATH, 'lib/org-mode-parser.js')) \
  .replace('return OrgQuery;', '\n\n' + plugin_tohtml + '\n\nreturn OrgQuery;') \
  .replace('var util=require(\'util\');', '') \
  .replace('var fs=require("fs");', '') \
  .replace('require("underscore");', 'require("$:/plugins/tiddlywiki/org-mode-parser/underscore.js");')

tiddlers[BASE_NAMESPACE+"/wrapper.js"]["text"]         = open('orgwrapper.js').read()
tiddlers[BASE_NAMESPACE+"/underscore.js"]["text"]      = get_content(UNDERSCORE_JS_PATH)

HEADER = '''\
author: %(author_name)s
created: %(time_create)s
dependents: 
description: Org-mode plugin wrapping org-mode-parser.js
plugin-type: plugin
title: $:/plugins/tiddlywiki/orgmode
type: application/json
version: 5.1.7

'''%dict(
    author_name = 'whacked',
    time_create = datetime.datetime.now().strftime('%Y%m%d%H%M%S%f')[:-3],
)

txt = json.dumps(out, indent=2)

if '-save' in sys.argv:
    # give it path to dir that is passed to tiddlywiki in e.g.
    # tiddlywiki MYDIR --server
    # = MYDIR here
    twdir = sys.argv[sys.argv.index('-save')+1]
    with open(p.join(twdir, 'tiddlers', '$__plugins_tiddlywiki_orgmode.tid'), 'w') as ofile:
        ofile.write(HEADER + txt)
        print('OK: wrote %s' % ofile.name)
else:
    print(HEADER + txt)

