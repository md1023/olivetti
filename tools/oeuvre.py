#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import eyed3

def get_mp3s(folder="."):
    mp3s = []
    for path, subdirs, files in os.walk(folder):
        mp3s.extend(
            [os.path.join(path, name) for name in files if name[-4:] == ".mp3"])
    return mp3s

def get_song_info(mp3s, predicate=lambda:None, chain=lambda:None, fields=("artist", "title")):
    for name in mp3s:
        mp3 = eyed3.load(name)
        song = []
        for f in (getattr(mp3.tag, f, None) for f in fields):
            if predicate(f):
                song += chain(f),
        yield name, song

if __name__ == "__main__":
    mp3s = get_mp3s()
    for s in get_song_info(
            mp3s,
            predicate = lambda s: s, # "," not in (s or ""),
            chain = lambda s: s): # s.split(",")):
        # continue
        print s
