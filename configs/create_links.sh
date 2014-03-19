#!/bin/bash
# Скрипт прилагается к конфигурационным файлам из общего хранилища
# и создаёт мягкие ссылки на эти файлы в директории откуда запущен.
# Его следует запускать из папки пользователя по умолчанию
# check if path to configuration files exists
if [ ! -d $1 ] || [[ $# -eq 0 ]] ; then
    echo -e "Specify path to configuration files like this:\n~/olivetti/configs/create_links.sh ~/olivetti/configs"
    exit 2
fi

echo -e "Links to files from:\n  $1\nWill be created in:\n  `pwd`\nCreating links:"

# put link of each file from specified directory in current directory
# this script is ignored along . and .. directories 
for f in `ls -A --ignore='create_links.sh' $1`; do
    target="$1/$f"
    linkname="$f"
    echo "  $linkname -> $target"
    if [ $# -eq 2 ] && [ $2 == "--dry" ] ; then
	continue
    fi
    ln -sfb $target $linkname
done
exit 0
