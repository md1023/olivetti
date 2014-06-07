console.debug("arm.js: script loaded");

$(function() {
    console.debug("arm.js: dom loaded");
    var body = $("body");
    body.css("display", "none");
    var header = $("<h2 class=\"webpage\">").text("Simon's weblog");
    body.prepend(header);
    body.fadeIn(400);
    links = {
    	previous: "",
    	up: "",
    	next: "",
    	home: " href=\"http://rocket-help.ru/index.html\"",
    	notepad: " href=\"http://rocket-help.ru/blog/index.html\"",
    	mail: " href=\"mailto:simno@rocket-help.ru\""
    };
    names = "previous up next home notepad mail".split(" ");
    footer_body = "";
    names.forEach(function(name) {
	footer_body += "<td><a" + links[name] + "><img src=\"../images/" +
	    name + ".png\"./></a></td>";
	if (name=="next")
	    footer_body += "<td class=\"nav-bar\"><h2>Simon\'s Weblog Navigation</h2></td>";
    });
    footer = "<footer><table><tbody><tr>" + footer_body +
	"</tr></tbody></table></footer>";
    body.append(footer);
});
