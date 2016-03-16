import sys
import os.path as p
import datetime
import json
import urllib2

PUBLISHER_NAME = 'whacked'

def get_content(path):
    if path.startswith('http'):
        print('getting path: %s' % path)
        res = urllib2.urlopen(path)
        return res.read()
    else:
        # assume local file
        if not p.exists(path):
            raise IOError('Could not locate file: %s'%path)
        else:
            return open(path).read()
    raise ValueError('failed to get content for given path: %s'%path)

BASE_NAMESPACE = "$:/plugins/%s/org-js" % PUBLISHER_NAME
out = {
    "tiddlers": {
        "$:/language/Docs/Types/text/org": {
            "title": "$:/language/Docs/Types/text/org",
            "description": "org mode",
            "name": "text/org"
        },
        # the actual parser library
        (BASE_NAMESPACE + "/org.js"): {
            "type": "application/javascript",
            "title": BASE_NAMESPACE + "/org.js",
            "module-type": "library",
        },
        # interaction/glue code for TW
        (BASE_NAMESPACE + "/wrapper.js"): {
            "title": BASE_NAMESPACE + "/wrapper.js",
            "type": "application/javascript",
            "module-type": "parser"
        }
    }
}


tiddlers = out["tiddlers"]
# NOTE: the export hack is necessary for the module to load properly in TW
tiddlers[BASE_NAMESPACE+"/org.js"]["text"] = \
  get_content("https://raw.githubusercontent.com/mooz/org-js/master/org.js") \
  .replace('var Org = ', '', 1) \
  .replace('var exports = {};', '', 1)
tiddlers[BASE_NAMESPACE+"/wrapper.js"]["text"] = open('orgwrapper.js').read()

HEADER = '''\
author: %(author_name)s
created: %(time_create)s
dependents: 
description: Org-mode plugin wrapping org.js
plugin-type: plugin
title: $:/plugins/%(author_name)s/orgmode
type: application/json
version: 0.0.1

'''%dict(
    author_name = PUBLISHER_NAME,
    time_create = datetime.datetime.now().strftime('%Y%m%d%H%M%S%f')[:-3],
)

txt = json.dumps(out, indent=2)

if '-save' in sys.argv:
    # give it path to dir that is passed to tiddlywiki in e.g.
    # tiddlywiki MYDIR --server
    # = MYDIR here
    twdir = sys.argv[sys.argv.index('-save')+1]
    with open(p.join(twdir, 'tiddlers', '$__plugins_%s_orgmode.tid'%PUBLISHER_NAME), 'w') as ofile:
        ofile.write(HEADER + txt)
        print('OK: wrote %s' % ofile.name)
else:
    print(HEADER + txt)

