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

def convert_seconds(time):
        minutes = str(time / 60).zfill(2)
        seconds = str(time - 60 * (time / 60)).zfill(2)
        return "%s:%s" % (minutes, seconds)

def get_song_info(mp3s, # gen,
                  predicate=lambda:None,
                  chain=lambda:None,
                  fields=("artist", "title")):
    for name in mp3s:
        mp3 = eyed3.load(name)
        info = (name,)
        for f in (getattr(mp3.tag, f, None) for f in fields):
            info += (f,)
        info += (convert_seconds(mp3.info.time_secs),)
        yield info

def split_song_times(songs):
    # [["zzz" if j<0 or j>=len(l) else l[j] for j in (i-1, i, i+1)] for i in range(len(l))]
    # [['zzz', 1, 2], [1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 'zzz']]
    for s in songs:
        if re.search("%(time)s" % CONSTS, s):
            time = re.search("(%(time)s+)(?:-)?(%(time)s+)? ?(.*)" % CONSTS, s).groups()
            yield time # tuple(t for t in time if t))

def split_song_name(info):
    artist, name = info[1:3]
    if artist and name:  # and re.search(): check if time is in name!
        comma = lambda s: s.split(",") if "," in s else s
        artists = comma(artist)
        songs = comma(name)
        times = list(split_song_times(songs))
        print artists, "-", songs, "", times, "\n"

if __name__ == "__main__":
    mp3s = get_mp3s()
    for s in get_song_info(mp3s):
        split_song_name(s)
