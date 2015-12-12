(function(){
  /*jslint node: true, browser: true */
  /*global $tw: false */
  "use strict";

  function trim(s) {
    return s.replace(/^\s+/, "").replace(/\s+$/, "");
  }

  function parse_header(text) {
    /* read header lines and look for var def directives;
       return an object that contains:
       `env`: the env object
       `body`: the header-stripped text

       the directive format follows org-mode:
       see http://orgmode.org/manual/var.html
       :var `name`=`transclusion-target`
       
       e.g.
       :var home_tiddler=Home
    */
    var out = {env: {}, body: null};
    var code_buffer = [];
    var spl = text.split("\n");
    var idx = 0;
    
    var match_head = new RegExp("^/{2,}\\s*:var");
    var match_var = new RegExp(":var\\b");
    for(;idx<spl.length;++idx) {
      var line = trim(spl[idx]);
      var strip_comment = line.replace(match_head, "");
      if(line == strip_comment) {
        break;
      } else {
        var split_on_var = strip_comment.split(match_var);
        if(split_on_var.length > 0) {
          split_on_var.forEach(function(match) {
            var pair = trim(match).split("=", 2);
            out.env[pair[0]] = pair[1];
          });
        }
      }
    };
    out.body = spl.slice(idx).join("\n");
    return out;
  }

exports["application/js"] = function(type,text,options) {
  /**
     COMMENTARY

     this function calls eval() on the input javascript, which is
     expected to be a function definition that takes a single
     argument, `env`, which contains custom information that your
     function might be interested in. Right now it only loads the text
     of other tiddlers; see `parse_header`.

     what the input arguments do:
     
     - `type` will be "application/js" (what the plugin exports)
     - `text` will be the actual text content of the tiddler that uses
        this content-type (i.e. selected "application/js" from the
        dropdown)
     - `options` is TiddlyWiki internal. Don't know much about it,
        although it is used to load external tiddler references via
        transclusion. More notes on transclusion later below.
     
     this plugin expects your function to return one of:
     
     - a `String`, which will be sent as raw HTML content of the tiddler
     - an `Array`, whose children are assumed to be widget specs.
     - an `Object`, which is assumed to be a single tiddlywiki widget
       specification. No sanity checking is done. The object gets
       wrapped into an `Array`.
     
     syntax: org-mode
     
     NOTE:
     
     AFAICT, TW expects the exported function to modify =this.tree=;
     the structure of which should be an =Array= that contains the
     widget specifications. I have not seen a unified/comprehensive
     documentation page on all available types. An introduction:
     
     http://tiddlywiki.com/dev/static/WidgetModules.html

     though for example it doesn't cover the "element". It looks like
     the lowest level type is the "string" type; i.e. you cannot set
     return a raw string; the raw string must be wrapped in a ={type:
     "string", value: $raw_string}=. Then, a =text= type is a text DOM
     node.

     If you return raw HTML inside a =text= type, it will escape the
     tags into =&lt;= and =&gt;=. To give raw HTML for the browser
     it looks like you return a ={type: "raw", html: $your_html}=

     EXAMPLE: "return" a stylized header element from the function:
     
     #+BEGIN_SRC javascript
     this.tree = [{type: "element",
                   tag: "h1",
                   attributes: {
                     "style": {type: "string", value: "border:2px solid blue;"}
                   },
                   children: [
                     {type: "text", text: "Hello, I am the header text!"}
                   ]}];
     #+END_SRC

     Translusion at render phase
     // transclusion widget specification syntax
     // ref https://github.com/Jermolene/TiddlyWiki5/blob/master/core/modules/parsers/wikiparser/rules/filteredtranscludeblock.js
     
     #+BEGIN_SRC javascript
     var transcludeNode = {
       type: "transclude",
       attributes: {tiddler: {type: "string",
                              value: "NAME_OF_SOURCE_TIDDLER"}}
     };
     this.tree = [transcludeNode]; // this does the single node transclusion
     
     // Another usage:
     this.tree = [{
       type: "tiddler",
       attributes: {
         tiddler: {type: "string", value: targetTitle}
	   	 },
	   	 children: [transcludeNode]
     }];
     #+END_SRC

     Retrieving the source tiddler content programmatically (pre-render phase)
     ref https://github.com/Jermolene/TiddlyWiki5/blob/master/core/modules/wiki.js
     
     function of interest: =getTextReference(textRef, defaultText, curTiddlerTitle)=
     This seems sufficient:
     =console.log(options.wiki.getTextReference("my_test_tiddler", "ERROR: no text", null));=
  */
  var parsed = parse_header(text);
  var env = {};
  Object.keys(parsed.env).forEach(function(varname) {
    env[varname] = options.wiki.getTextReference(parsed.env[varname], "ERROR: no text", null);
  });
  
  var call_string = "("+parsed.body+")(env)";
  var result = eval(call_string);
  switch(typeof result) {
  case "object":
    if(Array.isArray(result)) {
      this.tree = result;
    } else {
      this.tree = [result];
    }
    break;
  default:
  case "string":
    this.tree = [{type: "raw", html: ""+result}];
    break;
  }
};

})();

