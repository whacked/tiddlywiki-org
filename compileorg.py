import sys
import os.path as p
import datetime
import json

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


plugin_tohtml = open(p.expanduser('~/Desktop/org-mode-parser/lib/plugin-tohtml.js')).read()

tiddlers = out["tiddlers"]
tiddlers["$:/plugins/tiddlywiki/org-mode-parser/org-mode-parser.js"]["text"] = open(p.expanduser('~/Desktop/org-mode-parser/lib/org-mode-parser.js')).read().replace('//$$$INJECT$$$', plugin_tohtml)
tiddlers["$:/plugins/tiddlywiki/org-mode-parser/wrapper.js"]["text"] = open('orgwrapper.js').read()
tiddlers["$:/plugins/tiddlywiki/org-mode-parser/underscore.js"]["text"] = open(p.expanduser('~/Desktop/org-mode-parser/lib/underscore-min.js')).read()

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

