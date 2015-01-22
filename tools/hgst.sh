# Download other tool here:
# wget "http://www.pixelbeat.org/scripts/ansi2html.sh"

if [ ! "$1" ]; then
    echo "Supply working dir parameter"
    exit 1;
fi

OUTPUT=./hgst.html
if [ "$2" ]; then
    OUTPUT=$2
fi

find $1 -type d -name '-*' \
    -exec echo '</pre><div class="repository"><h1>{}</h1><pre class="files">' \; \
    -exec hg st {} \; \
    -exec echo '</pre><div class="diff"><pre>' \; \
    -exec hg diff --color true {} \; \
    -exec echo '</pre></div></div>' \; | \
/tmp/ansi2html.sh | \
    sed 's/&lt;/</g' | sed 's/&gt;/>/g' | sed 's/&quot;/"/g' | \
    sed 's;<style;<link rel="stylesheet" href="hgst.css" type="text/css">\n<style;' | \
    sed 's/<span class="bold">diff -r/<hr\/><span class="bold newdiff">diff -r/g' \
    > $OUTPUT
