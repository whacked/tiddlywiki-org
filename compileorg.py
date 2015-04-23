import sys
import os.path as p

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
        # interaction/glue code for TW
        "$:/plugins/tiddlywiki/org-mode-parser/wrapper.js": {
            "title": "$:/plugins/tiddlywiki/org-mode-parser/wrapper.js",
            "type": "application/javascript",
            "module-type": "parser"
        }
    }
}

out["$:/plugins/tiddlywiki/org-mode-parser/org-mode-parser.js"] = open(p.expanduser('~/Desktop/org-mode-parser/lib/org-mode-parser.js')).read()
out["$:/plugins/tiddlywiki/org-mode-parser/wrapper.js"] = open('orgwrapper.js').read()

txt = json.dumps(out, indent=2)

if '-save' in sys.argv:
    # give it path to dir that is passed to tiddlywiki in e.g.
    # tiddlywiki MYDIR --server
    # = MYDIR here
    twdir = sys.argv[sys.argv.index('-save')+1]
    with open(p.join(twdir, 'tiddlers', '$__plugins_tiddlywiki_orgmode.tid'), 'w') as ofile:
        ofile.write(txt)
        print('OK: wrote %s' % ofile.name)
else:
    print(txt)

