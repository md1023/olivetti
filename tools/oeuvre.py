#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import os
import eyed3
import datetime
import time
import subprocess
import sys

# TODO beauify this
NOT_DRY = False
if len(sys.argv) > 1:
    NOT_DRY = bool(sys.argv[1])

# avconv -i IMG_0111.MOV.mp3 -ss 00:02:24 -t 00:03:42 -b:a 256k -f mp3 IMG_0111_part1.MOV.mp3
# TODO -metadata should be optional if it is none or should be set by eyeD3
CONSTS=dict(time="[0-9]+:[0-9]",
            comm="avconv -i %(name)s.mp3 -ss %(start)s -t %(duration)s -b:a 256k -f mp3 -metadata title=\"%(song)s\" -metadata artist=\"%(artist)s\" -metadata comment=\"%(comment)s\" %(name)s_part%(piece)s.mp3")

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

def get_song_info(mp3s,
                  predicate=lambda:None,
                  chain=lambda:None,
                  fields=("artist", "title")):
    for name in mp3s:
        mp3 = eyed3.load(name)
        # TODO cut mp3 extension
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
    else:
        for s in songs:
            if re.search("%(time)s" % CONSTS, s):
                b, e, n = re.search("(%(time)s+)(?:-)?(%(time)s+)? ?(.*)" % CONSTS, s).groups()
                yield (b, e, n)
            else:
                yield (None, None, s)

# TODO songs get skipped, times split uncorrectly
def combine_song_times(songs, length):
    if not songs:
        return ""
    periods = []
    for i in range(len(songs) - 1):
        cb, ce, cn = songs[i]
        nb, ne, nn = songs[i+1]
        if i == 0 and not cb:
            cb = u"0:00"
        periods.append((cb, ce or nb, cn))
    lb, le, ln = songs[-1]
    periods.append((lb, le or length, ln))
    return periods

def seconds(s):
    t = time.strptime(s, "%M:%S")
    return int(datetime.timedelta(minutes=t.tm_min, seconds=t.tm_sec).total_seconds())

def generate_command(fname, subsongs, dry_run=not NOT_DRY):
    for piece, s in enumerate(subsongs):
        b, e, n, a = s
        if not b or not e:
            print ">>> skip", s
            continue
        fname_no_suffix = fname.split(".mp3")[0]
        part_time = "part%s %s-%s" % (piece, b, e)
        cmd = CONSTS["comm"] % dict(
            name=fname_no_suffix,
            start=seconds(b),
            duration=seconds(e) - seconds(b),
            piece=piece,
            song=n.strip() or part_time,
            artist=a.strip(),
            comment=fname_no_suffix[2:] + " " + part_time)
        print ">", cmd
        if not dry_run:
            subprocess.call(cmd, shell=True)

def split_song_name(info):
    # TODO split .mp3 in fname!
    fname, artist, name, length = info[0:4]

    if artist and name:  # and re.search(): check if time is in name!
        comma = lambda s: s.split(",") if "," in s else s
        artists = comma(artist)
        songs = comma(name)
        times = combine_song_times(list(split_song_times(songs)), length)
        times_with_artists = [t + (artists[i],) for i,t in enumerate(times)]
        print times_with_artists
        if times:
            command = generate_command(fname, times_with_artists)

if __name__ == "__main__":
    mp3s = get_mp3s()
    for s in get_song_info(mp3s):
        split_song_name(s)

