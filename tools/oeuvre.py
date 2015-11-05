#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import os
import eyed3
import datetime
import time
import subprocess
import sys

SEPARATOR=","
# TODO beautify this
NOT_DRY = False
if len(sys.argv) > 1:
    NOT_DRY = bool(sys.argv[1])

# avconv -i IMG_0111.MOV.mp3 -ss 00:02:24 -t 00:03:42 -b:a 256k -f mp3 IMG_0111_part1.MOV.mp3
CONSTS=dict(time="[0-9]+:[0-9]",
            comm="avconv -i %(name)s.mp3 -ss %(start)s -t %(duration)s -b:a 256k -f mp3 %(metadata)s %(outdir)s/%(name)s_part%(piece)s.mp3")

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

def gen_get_song_info(mp3s):
    for name in mp3s:
        mp3 = eyed3.load(name)
        # TODO cut mp3 extension
        info = tuple(getattr(mp3.tag, f, None) for f in ("artist", "title"))
        # skip if no info or single song
        # TODO single song must be cut if it has start time!
        if not info[1]:
            print "\nskipped:", name, "no names"
            continue
        if not SEPARATOR in info[1]:
            print "\nskipped:", name, "no separator"
            continue
        print "\n", name
        yield ( (name,) + info + (convert_seconds(mp3.info.time_secs),) )


def gen_split_song_times(songs):
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
    # may be obsolete (check for single songs)
    if not songs:
        return ""
    periods = []
    pe = None # previous song ending in case of many
    for i in range(len(songs) - 1):
        cb, ce, cn = songs[i]
        nb, ne, nn = songs[i+1]
        if i == 0 and not cb:
            cb = u"0:00"
        pe = ce or nb
        periods.append((cb, pe, cn))
    lb, le, ln = songs[-1]
    # single song does not need to be cut
    # if len(songs) == 1:
    #     lb = u"0:00"
    periods.append((lb or pe, le or length, ln))
    # print "periods", lb, le, ln, length
    return periods


def seconds(s):
    t = time.strptime(s, "%M:%S")
    return int(datetime.timedelta(minutes=t.tm_min, seconds=t.tm_sec).total_seconds())

def generate_command(fname, subsongs, dry_run=not NOT_DRY):
    success = True
    for piece, s in enumerate(subsongs):
        b, e, n, a = s
        if not b or not e:
            print "\nskipped", fname, "song", s, "hasn't beginning or end"
            success = False
            continue
        fname_no_suffix = fname.split(".mp3")[0]
        part_time = "part%s %s-%s" % (piece, b, e)

        metadata = dict(title=n.strip() or part_time,
                        artist=a.strip(),
                        comment=fname_no_suffix[2:] + " " + part_time)
        metadata = " ".join(
            ["-metadata %s=\'%s\'" % (k, v) for k,v in metadata.items() if v])

        cmd = CONSTS["comm"] % dict(
            name=fname_no_suffix,
            start=seconds(b),
            duration=seconds(e) - seconds(b),
            piece=piece,
            metadata=metadata,
            outdir="/tmp/oeuvre_cuts")
        print ">", cmd
        if not dry_run:
            subprocess.call(cmd, shell=True)
    return success


def split_song_name(fname, artist, name, length):
    comma = lambda s: s.split(SEPARATOR) if SEPARATOR in s else s
    assert name
    songs = comma(name)

    if not artist:
        artist = SEPARATOR * (len(songs) - 1)
    artists = comma(artist)

    # elaborate better solution
    try:
        assert len(songs) == len(artists), (songs, artists)
    except AssertionError:
        print "\nskipped:", songs, artists, "error in songs and artists",
        return

    songs = list(gen_split_song_times(songs))
    times = combine_song_times(songs, length)
    times_with_artists = [t + (artists[i],) for i,t in enumerate(times)]
    print "times:", times_with_artists
    if times:
        command = generate_command(fname, times_with_artists)
        if command:
            print "\nsuccessful:", fname


if __name__ == "__main__":
    mp3s = get_mp3s()
    for s in gen_get_song_info(mp3s):
        split_song_name(*s)
