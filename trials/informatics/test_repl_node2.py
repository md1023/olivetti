#!/usr/bin/env python3
# test_repl_node2.py

from unittest import TestCase, main
from repl_node2 import Parser, ParserError

class ParserTestCase(TestCase):
    def setUp(self):
        self.p = Parser

    # def test_1(self):
    #     self.assertEqual(self.p('3.5').Number(), 3.5)

    # def test_2(self):
    #     self.assertEqual(self.p('.5').Number(), 0.5)

    # def test_3(self):
    #     self.assertRaises(ParserError, self.p('3.').Number)

    # # def test_3(self):
    # #     self.assertEqual(self.p('3.').Number(), 3.0)

    # def test_4(self):
    #     self.assertRaises(ParserError, self.p('').Number)

    # def test_5(self):
    #     self.assertRaises(ParserError, self.p('.').Number)

    # def test_6(self):
    #     self.assertRaises(ParserError, self.p('trash').Number)

    # def test_7(self):
    #     self.assertRaises(ParserError, self.p('..').Number)

    # def test_8(self):
    #     self.assertRaises(ParserError, self.p('3..').Number)

    # def test_9(self):
    #     self.assertRaises(ParserError, self.p('.3.').Number)

    # def test_10(self):
    #     self.assertRaises(ParserError, self.p('..3').Number)

    def test_11(self):
        self.assertEqual(self.p('356243.5').Number(), 356243.5)

    def test_12(self):
        self.assertEqual(self.p('5.356243').Number(), 5.356243)
        
    def test_13(self):
        self.assertEqual(self.p('.356243').Number(), 0.356243)
        
if __name__ == '__main__':
    main()
