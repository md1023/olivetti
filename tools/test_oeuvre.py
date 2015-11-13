#!/usr/bin/env python
# -*- coding: utf-8 -*-

import unittest
from oeuvre import comma, gen_split_song_times, combine_song_times

class TestOeuvreMP3Tags(unittest.TestCase):
    def setUp(self):
        self.song = "a, 1:12 b, 2:30 c"

    def test_split_song_times(self):
        self.assertEqual(
            [(None, None, 'a'), ('1:12', None, 'b'), ('2:30', None, 'c')],
            list(gen_split_song_times(comma(self.song))))

    def test_combine_song_times(self):
        song_end = "3:54"
        self.assertEqual(
            [('0:00', '1:12', 'a'), ('1:12', '2:30', 'b'), ('2:30', '3:54', 'c')],
            combine_song_times(
                list(gen_split_song_times(comma(self.song))),
                song_end))

    def test_wrong_combine_song_times(self):
        song = "a, 1:12 b, c 2:30"
        song_end = "3:54"
        self.assertRaises"
        self.assertEqual(
            [('0:00', '1:12', 'a'), ('1:12', '2:30', 'b'), ('2:30', '3:54', 'c')],
            combine_song_times(
                list(gen_split_song_times(comma(song))),
                song_end))

if __name__ == '__main__':
    unittest.main()
