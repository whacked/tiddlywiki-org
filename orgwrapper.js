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
    // since org-mode-parser's nodes are segmented on the headline,
    // if has a headline, create an extra element just for it
    if(nodes[index].headline.length > 0) {
      var headline_widget = {type: "element",
                             // cap the heading level at 6
                             tag: "h"+(nodes[index].level < 5 ? 6-nodes[index].level : 6)};
      headline_widget.children = [nodes[index].headline];
      results.push(headline_widget);
    }
    
		results.push(transformNode(nodes[index]));
	}
	return results;
}

function processText(text) {
    var tag = null;
    // horizontal rule
    if(text.test(/-{5,}\s*$/)) {
      return [tag, ""];
    }
    
    //  // begin source
    //  /^#\+begin_src\s+([^ ]+)(.*)/i,
    //  // end source
    //  /^#\+end_src\s*/i,

    //  // image file, double bracket
    //  [/\[\[file:\s*([^ ]+)\.(?:PNG|JPG|BMP|GIF|TIFF|SVG)\]\]/i],
    //  // image file, no bracket
    //  [/(?:^|[^[])file:([^ ]+)\.(?:PNG|JPG|BMP|GIF|TIFF|SVG)(?:[^\]]|$)/i],
    //  // hyperlink with description
    //  [/\[\[(?:file:)?(.*?)\]\[(.*?)\]\]/i],
    //  // hyperlink without description
    //  [/\[\[(?:file:)?(.*?)\]\]/i],

    var rtn = [];
    var remainder = line;
    var candidate_list = [
      // code
      [/=([^=]+?)=/, "code"],
      // verbatim
      [/~([^~]+?)~/, "code"],
      // italic
      [/\/(.+?)\//, "em"],
      // bold
      [/\*([^\*]+?)\*/, "strong"],
      // underline
      [/_([^_]+?)_/, "u"],
    ];
    while(remainder.length > 0) {
      for(var i=0;i<candidate_list.length;++i) {
        var test = candidate_list[i][0],
            tag = candidate_list[i][1];
        var m = remainder.match(test);
        if(m) {
          rtn.push([tag, m[0]]);
          remainder = remainder.substr(m.index);
        }
      }
    }

    // lists
    //// definition list
    line1 = text.replace(/- +([^ ]+) :: (.+)/, '<dt>$1</dt><dd>$2</dd>')
    if(line1 != text) return [self.DL, line1]
    //// ordered
    line1 = text.replace(/\d+[\.)] (.+)/, '<li>$1</li>')
    if(line1 != text) return [self.OL, line1]
    //// unordered
    line1 = text.replace(/[-+] (.+)/, '<li>$1</li>')
    if(line1 != text) return [self.UL, line1]

    return [tag, line1];
}




function transformNode(node) {
	if($tw.utils.isArray(node)) {
		var p = 0,
			widget = {type: "element", tag: node[p++]};
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
  this.tree = transformNodes(orgList);
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

