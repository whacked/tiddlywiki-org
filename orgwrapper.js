/*\
title: $:/plugins/tiddlywiki/org/wrapper.js
type: application/javascript
module-type: parser

Wraps up the org-mode-parser parser for use in TiddlyWiki5
\*/
(function(){

/*jslint node: true, browser: true */
/*global $tw: false */
//"use strict";

  x = require;
  
Org = require("$:/plugins/tiddlywiki/org-js/org.js");

exports["text/org"] = function(type,text,options) {
  var orgParser = new Org.Parser();
  var orgDocument = orgParser.parse(text);
  var orgHTMLDocument = orgDocument.convert(Org.ConverterHTML, {
    headerOffset: 0,
    exportFromLineNumber: false,
    suppressSubscriptHandling: false,
    suppressAutoLink: false
  });
  this.tree = [{type: "raw", html: orgHTMLDocument.contentHTML}];
};

})();

