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
    if isinstance(songs, basestring):
        if re.search("%(time)s" % CONSTS, songs):
            b, e, n = re.search("(%(time)s+)(?:-)?(%(time)s+)? ?(.*)" % CONSTS, songs).groups()
            yield (b, e, n)
        # else:
        #     yield (None, None, songs)
    else:
        for s in songs:
            if re.search("%(time)s" % CONSTS, s):
                b, e, n = re.search("(%(time)s+)(?:-)?(%(time)s+)? ?(.*)" % CONSTS, s).groups()
                yield (b, e, n)
            # else:
            #     yield (None, None, s)

# rewrite for pairs instead of triples
def combine_song_times(songs, length):
    if len(songs) == 1:
        print ">>1", songs
    if len(songs) == 2:
        print ">>2", songs
    pairs = []
    for i in range(1, len(songs) - 1):
        prev = songs[i-1]
        pos = songs[i]
        next_ = songs[i+1]
        pairs.append([prev, pos, next_])
    if pairs:
        print ">>>", pairs

def split_song_name(info):
    artist, name, length = info[1:4]
    
    if artist and name:  # and re.search(): check if time is in name!
        comma = lambda s: s.split(",") if "," in s else s
        artists = comma(artist)
        songs = comma(name)
        times = list(split_song_times(songs))
        print "\n", artists, "-", songs
        combine_song_times(times, length)

if __name__ == "__main__":
    mp3s = get_mp3s()
    for s in get_song_info(mp3s):
        split_song_name(s)
