/*\
title: $:/plugins/tiddlywiki/org/wrapper.js
type: application/javascript
module-type: parser

Wraps up the org-mode-parser parser for use in TiddlyWiki5
\*/
(function(){

/*jslint node: true, browser: true */
/*global $tw: false */
"use strict";

var Org = require("$:/plugins/whacked/org-js/org.js");

exports["text/org"] = function(type,text,options) {

  // preprocess:
  // - pre-apply transclusion directives:
  //   #+INCLUDE: "source_file.org" :lines N-M
  //   for now we will just inject it completely
  // - change local_link to #local_link for TW intralinking
  var includePattern = /\s*#\+INCLUDE:\s*"?([^ "]+)"?\s*(?::lines\s+(\d+)[-~](\d+))?\s*?/;
  
  var arrPreproc = [];
  text.split("\n").forEach(function(origLine) {
    var procLine;
    
    var transcludeMatch = origLine.match(includePattern);
    if(Array.isArray(transcludeMatch)) {
      var sourceFile = transcludeMatch[1],
          lineBegin = transcludeMatch[2], // ignored for now
          lineEnd = transcludeMatch[3], // ignored for now
          sourceTiddler = sourceFile.replace(/\.[^\.]+$/, ""); // strip extension
      procLine = options.wiki.getTextReference(sourceTiddler, "ERROR: no text", null);
    } else {
      // note the "#" --> this is needed for TW's href for local tiddlers
      // (otherwise the link will be seen as e.g. http://localhost:8080/link
      procLine = origLine.replace(/\[\[(?:file:)([^\/.])(.+?)\.tid\](\[.+?\])?\]/g, "[[#$1$2]$3]")
    }
    arrPreproc.push(procLine);
  });
  
  var orgParser = new Org.Parser();
  var orgDocument = orgParser.parse(arrPreproc.join("\n"));
  var orgHTMLDocument = orgDocument.convert(Org.ConverterHTML, {
    headerOffset: 0,
    exportFromLineNumber: false,
    suppressSubscriptHandling: false,
    suppressAutoLink: false
  });
  this.tree = [{type: "raw", html: orgHTMLDocument.contentHTML}];
};

})();

