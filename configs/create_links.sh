#!/bin/bash
# Скрипт прилагается к конфигурационным файлам из общего хранилища
# и создаёт мягкие ссылки на эти файлы в директории откуда запущен.
# Его следует запускать из папки пользователя по умолчанию
# check if path to configuration files exists

# locate script location and destination folders
DIR="$( cd "$( dirname "$0" )" && pwd )"

echo -e "Script dir: $DIR\nCreating links:"

# put link of each file from specified directory in current directory
# this script is ignored along . and .. directories 
for f in `ls -A --ignore='create_links.sh' $DIR`; do
    target="$DIR/$f"
    linkname="$HOME/$f"
    echo "  $linkname -> $target"
    if [ $# -eq 1 ] && [ $1 == "--dry" ] ; then
	continue
    fi
    if [ -d $linkname ]; then
	echo "backup folder: $linkname~"
	mv $linkname "$linkname~"
    fi
    ln -sfb $target $linkname
    # cp -psb $target $linkname
done
exit 0
