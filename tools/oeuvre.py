#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import os
import eyed3

CONSTS=dict(time="[0-9]+:[0-9]")

def get_mp3s(folder="."):
    mp3s = []
    for path, subdirs, files in os.walk(folder):
        mp3s.extend(
            [os.path.join(path, name) for name in files if name[-4:] == ".mp3"])
    return mp3s

def get_song_info(mp3s, # gen,
                  predicate=lambda:None,
                  chain=lambda:None,
                  fields=("artist", "title")):
    for name in mp3s:
        mp3 = eyed3.load(name)
        info = (name,)
        for f in (getattr(mp3.tag, f, None) for f in fields):
            info += (f,)
        yield info

def split_song_name(info):
    artist, name = info[1:]
    if artist and name:  # and re.search(): check if time is in name!
        comma = lambda s: s.split(",") if "," in s else s
        print comma(artist), "-", comma(name), \
            re.search("(%(time)s+)(?:-)?(%(time)s+)? (.*)" % CONSTS, name).groups()

if __name__ == "__main__":
    mp3s = get_mp3s()
    for s in get_song_info(mp3s):
        split_song_name(s)
