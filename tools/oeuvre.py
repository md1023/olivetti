#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import eyed3

mp3s = []

for path, subdirs, files in os.walk("."):
    mp3s.extend(
        [os.path.join(path, name) for name in files if name[-4:] == ".mp3"])

for name in mp3s:
    f = eyed3.load(name)
    artist = getattr(f.tag, "artist", None)
    title = getattr(f.tag, "title", None)
    if "," in str(artist):
        artist = artist.split(",")
    if "," in str(artist):
        title = title.split(",")
    print name, artist or "", title or ""

# all mp3s' tags should contain comma as separator
