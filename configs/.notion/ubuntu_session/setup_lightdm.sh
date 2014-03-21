#!/bin/bash

#check root access
if [ "$(id -u)" != "0" ]; then
   echo "This script must be run as root" 1>&2
   # exit 1
fi

# locate script location and destination folders
DIR="$( cd "$( dirname "$0" )" && pwd )"
DEST="/usr/share"

exit 1
# perform copy
cp ./xsessions.notion.desktop $DEST/xsessions/notion.desktop
cp ./applications.notion.desktop $DEST/applications/notion.desktop
cp ./gnome-session.notion.session $DEST/gnome-session/sessions/notion.session
cp ./custom_notion_badge.png $DEST/unity-greeter/custom_notion_badge.png
