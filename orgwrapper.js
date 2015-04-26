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

var orgModeParser = require("$:/plugins/tiddlywiki/org-mode-parser/org-mode-parser.js");

function transformNodes(nodes) {
  var results = [];
  for(var index=0; index<nodes.length; index++) {
    results.push(transformNode(nodes[index]));
  }
  return results;
}

function transformNode(node) {
  if($tw.utils.isArray(node)) {
    var p = 0,
        widget = {type: "element", tag: node[p++], attributes: {}};
    if(!$tw.utils.isArray(node[p]) && typeof(node[p]) === "object") {
      widget.attributes = {};
      $tw.utils.each(node[p++],function(value,name) {
        widget.attributes[name] = {type: "string", value: value};
      });
    }
    widget.children = transformNodes(node.slice(p++));
    // Massage images into the image widget
    if(widget.tag === "img") {
      widget.type = "image";
      if(widget.attributes.alt) {
        widget.attributes.tooltip = widget.attributes.alt;
        delete widget.attributes.alt;
      }
      if(widget.attributes.src) {
        widget.attributes.source = widget.attributes.src;
        delete widget.attributes.src;
      }
    }
    return widget;
  } else {
    return {type: "text", text: node};
  }
}

var orgRenderer = function(type,text,options) {
  var orgList = orgModeParser.parseBigString(text);
  var oq = new orgModeParser.OrgQuery(orgList);
  this.tree = transformNodes(oq.toSimpleTree());
};

/*

[ 'html',
  [ 'p', 'something' ],
  [ 'h1',
    'heading and ',
    [ 'strong', 'other' ] ] ]

*/

exports["text/org"] = orgRenderer;

})();

