console.debug("hotkeys.js: script loaded");

$(function () {
    console.debug("hotkeys.js: dom loaded");
    var form = $("#save_combination");

    if (!$('#tag_canvas').tagcanvas({
	textColour: '#000',
	textFont: 'comfortaa, verdana, sans-serif',
	outlineThickness: 1,
	maxSpeed: 0.03,
	depth: 0.75,
	hideTags: false,
	txtOpt: true,
	wheelZoom: false,
	dragControl: true
    }, "tag_list")) {
	// TagCanvas failed to load
	console.debug("canvas failed");
	$('#tag_canvas').hide();
    };
});
