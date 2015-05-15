#!/bin/bash
# Download other tool here:
# wget "http://www.pixelbeat.org/scripts/ansi2html.sh" && chmod +x ansi2html.sh
# SEARCH_NAME='-name repo' /home/m-nikolaev/olivetti/tools/hgst.sh /home/m-nikolaev/work
# script is launched from /usr/lib/python2.7/dist-packages/mercurial/hgweb/hgwebdir_mod.py
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
echo "<div class=\"content\">
<base target=\"_parent\">
<span class=\"redmine\">
  <strong>Warning:</strong> SSH <u>passphrase</u> must be empty due to <em>\$ROWS</em> generation.
</span><br/>
" > $TMP

# build redmine and jenkins references
JOBS_DIR=/var/lib/jenkins/jobs/
ROWS=`ssh $JENKINS_SERVER 'for f in \`find -L /var/lib/jenkins/jobs/ -path "*lastSuccessful/archive*" -name "last_issues.html"\`; do
    echo "<tr>\`cat $f\`</tr>"
done;'`
if [[ -n "$ROWS" ]]; then
    echo "<table class=\"table issues\">
</br>
<tr><th>Job</th><th>Redmine</th><th>Rev</th></tr>
$ROWS
</table>" >> $TMP
fi

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

# iframe counterpart lies in mercurial.js within boundstate
cat $TMP | \
    $DIR/ansi2html.sh | \
    sed 's/&lt;/</g' | sed 's/&gt;/>/g' | sed 's/&quot;/"/g' | \
    sed '/<pre>/d' | \
    sed 's;<style;<link rel="stylesheet" href="styles.css" type="text/css"><link rel="stylesheet" href="hgst.css" type="text/css">\n<style;' | \
    sed 's/<span class="bold">diff -r/<hr\/><span class="bold newdiff">diff -r/g' | \
    sed 's#<body#<body onload=\"window.addEventListener('\''message'\'', '\
'function(e) {e.source.postMessage(this.document.body.scrollHeight, '\''http://192.168.200.83:8008'\''); }, false);"#' \
    > $OUTPUT
rm $TMP
