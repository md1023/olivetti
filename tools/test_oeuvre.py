#!/usr/bin/env python
# -*- coding: utf-8 -*-

import unittest
from oeuvre import gen_split_song_times

class TestOeuvreMP3Tags(unittest.TestCase):

    def test_split_song_times(self):
        print list(gen_split_song_times("a, 1:12 b, 2:30 c"))

if __name__ == '__main__':
    unittest.main()
