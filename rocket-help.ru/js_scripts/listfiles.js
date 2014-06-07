console.debug("listfiles.js: script loaded");

$(document).ready(
    function() {
	console.debug("listfiles.js: dom loaded");
	var container = $("#file-list");
	$.ajax({
	    type: "POST",
	    url: "../php_scripts/list_files.php",
	    success: function(data) {
		console.debug(data);
		var ul = $("<ul style='display:none'>").appendTo(container);
		$.each(data, function(link, header) {
		    $("<a>").attr("href", link).text(header).appendTo(
			$("<li>").appendTo(ul));
		});
		ul.fadeIn(275);
	    },
	    dataType: "json"
	});
    });
