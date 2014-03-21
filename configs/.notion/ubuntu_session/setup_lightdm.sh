#!/bin/bash

#check root access
if [ "$(id -u)" != "0" ]; then
   echo "This script must be run as root" 1>&2
   # exit 1
fi

# locate script location
DIR="$( cd "$( dirname "$0" )" && pwd )"
echo "GO", $DIR

exit 1
# perform copy
cp ./xsessions.notion.desktop /usr/share/xsessions/notion.desktop
cp ./applications.notion.desktop /usr/share/applications/notion.desktop
cp ./gnome-session.notion.session /usr/share/gnome-session/sessions/notion.session
cp ./custom_notion_badge.png /usr/share/unity-greeter/custom_notion_badge.png
