#!/bin/bash
# Download other tool here:
# wget "http://www.pixelbeat.org/scripts/ansi2html.sh" && chmod +x ansi2html.sh

if [ ! "$1" ]; then
    echo "Supply working dir parameter"
    exit 1;
fi

OUTPUT=./hgst.html
if [ "$2" ]; then
    OUTPUT=$2
fi

HANDLER=

TMP=/tmp/hgst.tmp
touch $TMP
echo "<div class=\"content\">" > $TMP
for i in `find $1 -type d -name "-*"`; do
    REPO=`basename $i`
    SHORT_STATUS=`hg diff --repository $i --stat --color true`
    if [ -z "$SHORT_STATUS" ]; then
	# echo "$REPO no changes"
	continue
    fi;
    DIFFS=`hg diff --repository $i --color true`
    echo "
<div class=\"repository sources\">
  <h2>$REPO</h2>
    <pre class=\"diffs\">$DIFFS</pre>
    <pre class=\"files\">$SHORT_STATUS</pre>
</div>" >> $TMP
done;
echo "</div>" >> $TMP
cat $TMP | \
./ansi2html.sh | \
    sed 's/&lt;/</g' | sed 's/&gt;/>/g' | sed 's/&quot;/"/g' | \
    sed '/<pre>/d' | \
    sed 's;<style;<link rel="stylesheet" href="hgst.css" type="text/css">\n<style;' | \
    sed 's/<span class="bold">diff -r/<hr\/><span class="bold newdiff">diff -r/g' | \
    sed 's#<body#<body onload=\"window.addEventListener('\''message'\'', '\
'function(e) {e.source.postMessage(this.document.body.scrollHeight, '\''http://localhost:8008'\''); }, false);"#' \
    > $OUTPUT
rm $TMP
