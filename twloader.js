const path = require("path"),
      tiddlywiki = require("tiddlywiki"),
      tiddlywiki_boot_path = require.resolve("tiddlywiki"),
      tiddlywiki_root = path.dirname(path.dirname(tiddlywiki_boot_path));

function expandUserHome(p) {
    return p.replace(/^~/, process.env.HOME);
}

// forces no-browser
function loadTiddlyWiki(wikiPath) {
	// ref $TiddlyWikiRepoRoot/boot/boot.js at _boot top
    var $tw0 = require(path.join(tiddlywiki_root,
                                 "boot", "bootprefix.js")).bootprefix({
                                     browser: null
                                 });
	// ref $TiddlyWikiRepoRoot/boot/boot.js:startup()
    var $tw = tiddlywiki.TiddlyWiki($tw0);
    $tw.boot.argv = [];
    $tw.boot.wikiPath = expandUserHome(wikiPath);

	// Get the URL hash and check for safe mode
	$tw.locationHash = "#";
	if($tw.browser && !$tw.node) {
		if(location.hash === "#:safe") {
			$tw.safeMode = true;
		} else {
			$tw.locationHash = $tw.utils.getLocationHash();
		}
	}
	// Initialise some more $tw properties
	$tw.utils.deepDefaults($tw,{
		modules: { // Information about each module
			titles: Object.create(null), // hashmap by module title of {fn:, exports:, moduleType:}
			types: {} // hashmap by module type of hashmap of exports
		},
		config: { // Configuration overridables
			pluginsPath: "../plugins/",
			themesPath: "../themes/",
			languagesPath: "../languages/",
			editionsPath: "../editions/",
			wikiInfo: "./tiddlywiki.info",
			wikiPluginsSubDir: "./plugins",
			wikiThemesSubDir: "./themes",
			wikiLanguagesSubDir: "./languages",
			wikiTiddlersSubDir: "./tiddlers",
			wikiOutputSubDir: "./output",
			jsModuleHeaderRegExpString: "^\\/\\*\\\\(?:\\r?\\n)((?:^[^\\r\\n]*(?:\\r?\\n))+?)(^\\\\\\*\\/$(?:\\r?\\n)?)",
			fileExtensionInfo: Object.create(null), // Map file extension to {type:}
			contentTypeInfo: Object.create(null), // Map type to {encoding:,extension:}
			pluginsEnvVar: "TIDDLYWIKI_PLUGIN_PATH",
			themesEnvVar: "TIDDLYWIKI_THEME_PATH",
			languagesEnvVar: "TIDDLYWIKI_LANGUAGE_PATH",
			editionsEnvVar: "TIDDLYWIKI_EDITION_PATH"
		},
		log: {}, // Log flags
		unloadTasks: []
	});
	if(!$tw.boot.tasks.readBrowserTiddlers) {
		// For writable tiddler files, a hashmap of title to {filepath:,type:,hasMetaFile:}
		$tw.boot.files = Object.create(null);
		// System paths and filenames
		$tw.boot.bootPath = path.dirname(module.filename);
		$tw.boot.corePath = path.resolve($tw.boot.bootPath,"../core");
		// Read package info
		$tw.packageInfo = require(path.join(tiddlywiki_root, "package.json"));
		// Check node version number
		if(!$tw.utils.checkVersions(process.version.substr(1),$tw.packageInfo.engines.node.substr(2))) {
			$tw.utils.error("TiddlyWiki5 requires node.js version " + $tw.packageInfo.engines.node);
		}
	}
	// Add file extension information
	$tw.utils.registerFileType("text/vnd.tiddlywiki","utf8",".tid");
	$tw.utils.registerFileType("application/x-tiddler","utf8",".tid");
	$tw.utils.registerFileType("application/x-tiddlers","utf8",".multids");
	$tw.utils.registerFileType("application/x-tiddler-html-div","utf8",".tiddler");
	$tw.utils.registerFileType("text/vnd.tiddlywiki2-recipe","utf8",".recipe");
	$tw.utils.registerFileType("text/plain","utf8",".txt");
	$tw.utils.registerFileType("text/css","utf8",".css");
	$tw.utils.registerFileType("text/html","utf8",[".html",".htm"]);
	$tw.utils.registerFileType("application/hta","utf16le",".hta",{deserializerType:"text/html"});
	$tw.utils.registerFileType("application/javascript","utf8",".js");
	$tw.utils.registerFileType("application/json","utf8",".json");
	$tw.utils.registerFileType("application/pdf","base64",".pdf",{flags:["image"]});
	$tw.utils.registerFileType("application/zip","base64",".zip");
	$tw.utils.registerFileType("image/jpeg","base64",[".jpg",".jpeg"],{flags:["image"]});
	$tw.utils.registerFileType("image/png","base64",".png",{flags:["image"]});
	$tw.utils.registerFileType("image/gif","base64",".gif",{flags:["image"]});
	$tw.utils.registerFileType("image/svg+xml","utf8",".svg",{flags:["image"]});
	$tw.utils.registerFileType("image/x-icon","base64",".ico",{flags:["image"]});
	$tw.utils.registerFileType("application/font-woff","base64",".woff");
	$tw.utils.registerFileType("audio/ogg","base64",".ogg");
	$tw.utils.registerFileType("video/mp4","base64",".mp4");
	$tw.utils.registerFileType("audio/mp3","base64",".mp3");
	$tw.utils.registerFileType("audio/mp4","base64",[".mp4",".m4a"]);
	$tw.utils.registerFileType("text/x-markdown","utf8",[".md",".markdown"]);
	// Create the wiki store for the app
	$tw.wiki = new $tw.Wiki();
	// Install built in tiddler fields modules
	$tw.Tiddler.fieldModules = $tw.modules.getModulesByTypeAsHashmap("tiddlerfield");
	// Install the tiddler deserializer modules
	$tw.Wiki.tiddlerDeserializerModules = Object.create(null);
	$tw.modules.applyMethods("tiddlerdeserializer",$tw.Wiki.tiddlerDeserializerModules);

    $tw.loadTiddlersNode();
    
	// Load the tiddlers from the wiki directory
	if($tw.boot.wikiPath) {
		$tw.boot.wikiInfo = $tw.loadWikiTiddlers($tw.boot.wikiPath);
	}

	// Unpack plugin tiddlers
	$tw.wiki.readPluginInfo();
	$tw.wiki.registerPluginTiddlers("plugin",$tw.safeMode ? ["$:/core"] : undefined);
	$tw.wiki.unpackPluginTiddlers();
	// Process "safe mode"
	if($tw.safeMode) {
		$tw.wiki.processSafeMode();
	}
	// Register typed modules from the tiddlers we've just loaded
	$tw.wiki.defineTiddlerModules();
	// And any modules within plugins
	$tw.wiki.defineShadowModules();
	// Make sure the crypto state tiddler is up to date
	if($tw.crypto) {
		$tw.crypto.updateCryptoStateTiddler();
	}

	return $tw;
};

module.exports = {
    loadTiddlyWiki: loadTiddlyWiki
}