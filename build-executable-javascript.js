fs = require("fs");
path = require("path");

if(process.argv.length < 3) {
  console.log("USAGE: __FILE__ $PATH_TO_TW_FOLDER");
  process.exit();
}

// http://tiddlywiki.com/static/PluginMechanism.html
var INFO = {
    VERSION: "0.0.1"
  , PLUGIN_NAME: "executable-js"
  , DESCRIPTION: "execute javascript"
  , AUTHOR: "nobody"
  , PUBLISHER: "nobody"
};
function tw_header() {
  return "\
author: "+INFO.AUTHOR+"\n\
created: "+(new Date().toISOString().slice(0,10))+"\n\
dependents: \n\
description: "+INFO.DESCRIPTION+"\n\
plugin-type: plugin\n\
title: $:/plugins/"+INFO.PUBLISHER+"/"+INFO.PLUGIN_NAME+"\n\
type: application/json\n\
version: "+INFO.VERSION+"\n\
\n\
";
}

fs.readFile("./executable-javascript.js", "utf8", function(err, txt) {
  if(err) {
    console.error(err);
    return console.log(err);
  }

  var out = {
    "tiddlers": {
      "$:/language/Docs/Types/application/js": {
        "description": "executable js code", 
        "name": "application/js",
        "title": "$:/language/Docs/Types/application/js"
      }
    }
  };
  out["tiddlers"]["$:/plugins/"+INFO.PUBLISHER+"/executable-javascript/executable-javascript.js"] = {
    "title": "$:/language/Docs/Types/application/js",
    "description": "executable javascript",
    "type": "application/javascript",
    "module-type": "parser",
    "text": txt
  };

  var output_text = tw_header() + JSON.stringify(out,null,4);
  var output_filepath = path.join(process.argv[2], 'tiddlers', '$__plugins_'+INFO.PUBLISHER+'_executable-javascript.tid');
  fs.writeFile(output_filepath, output_text, function(err) {
    if(err) {
      console.error("ERROR: " + err);
    }
    console.log("OK. Wrote to: "+output_filepath);
  });
});

