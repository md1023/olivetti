//console.debug("script loaded");



if (document.observe)
 document.observe('dom:loaded', pageLoaded);
else if (window.onload)
  window.onload = pageLoaded(" in IE");

function pageLoaded(s) {
 alert("page loaded" + s);
};





function createVariation(e) {
 Event.stop();
 if (e.keyCode == 13) {
  //alert(e.target.value);
  var el = T.table(T.tr(T.td("max")));
  $('form').appendChild(el);
 }
};