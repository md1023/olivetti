#!/bin/bash
# Download other tool here:
# wget "http://www.pixelbeat.org/scripts/ansi2html.sh" && chmod +x ansi2html.sh
# SEARCH_NAME='-name repo' /home/m-nikolaev/olivetti/tools/hgst.sh /home/m-nikolaev/work
LANG=en_US.UTF-8

if [ ! "$1" ]; then
    echo "Supply working dir parameter"
    exit 1;
fi

DIR="$( cd "$( dirname "$0" )" && pwd )"

OUTPUT=./hgst.html
if [ "$2" ]; then
    OUTPUT=$2
fi

TMP=/tmp/hgst.tmp
touch $TMP
echo "<div class=\"content\">" > $TMP

# build short summary
echo "<table class=\"ids\">" >> $TMP
echo "<tr><th>Name</th><th>Rev</th><th>Labels</th></tr>" >> $TMP
for REPO in `find $1 -type d $SEARCH_NAME -printf "%f\n"`; do
    echo "<tr><td><span class=\"repository\">$REPO</span></td>" >> $TMP
    echo "<td><span class=\"revision\">`hg id --repository $1/$REPO -n`</span></td><td>" >> $TMP
    PARAMETERS=("-b" "-t" "-B")
    FIELD=("branch" "tag" "bookmark")
    for i in {0..2} ; do
	echo "<span class=\"${FIELD[$i]}\">`hg id --repository $1/$REPO ${PARAMETERS[$i]}`</span>" >> $TMP
    done;
    echo "</td></tr>" >> $TMP
done;
echo "</table>" >> $TMP

# build diffs
for REPO in `find $1 -type d $SEARCH_NAME -printf "%f\n"`; do
    SHORT_STATUS=`hg diff --repository $1/$REPO --stat --color true`
    if [ -z "$SHORT_STATUS" ]; then
	echo "$REPO no changes"
	continue
    fi;
    DIFFS=`hg diff --repository $1/$REPO --color true`
    echo "
<div class=\"repository sources\">
  <h2>$REPO</h2>
    <pre class=\"diffs\">$DIFFS</pre>
    <pre class=\"files\">$SHORT_STATUS</pre>
</div>" >> $TMP
done;
echo "</div>" >> $TMP
cat $TMP | \
    $DIR/ansi2html.sh | \
    sed 's/&lt;/</g' | sed 's/&gt;/>/g' | sed 's/&quot;/"/g' | \
    sed '/<pre>/d' | \
    sed 's;<style;<link rel="stylesheet" href="hgst.css" type="text/css">\n<style;' | \
    sed 's/<span class="bold">diff -r/<hr\/><span class="bold newdiff">diff -r/g' | \
    sed 's#<body#<body onload=\"window.addEventListener('\''message'\'', '\
'function(e) {e.source.postMessage(this.document.body.scrollHeight, '\''http://localhost:8008'\''); }, false);"#' \
    > $OUTPUT
rm $TMP
