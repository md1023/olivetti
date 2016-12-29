#!/usr/bin/env python3
import re


class Printable:
    def __repr__(self):
        return "{0}[{1}]".format(self.__class__.__name__, self.value)


class Terminal(Printable):
    def __init__(self, value):
        self.value = value

    
class Digit(Terminal):
    regex = '[0-9]'


class Dot(Terminal):
    regex = '\.'


class End(Terminal):
    regex = '\$'


class TokenNotFound(Exception):
    pass


class RuleNotFound(Exception):
    pass


class Lexer:
    map = [Digit, Dot, End]
    
    def __init__(self, input):
        self.tokens = [self.tokenize(c) for c in input]

    def tokenize(self, character):
        for T in self.map:
            if re.match(T.regex, character):
                return T(character)
        raise TokenNotFound('unknown character \'{0}\''.format(character))


class Node(Printable):
    def __init__(self, value, subnodes=None):
        if subnodes:
            self.subnodes = subnodes
        self.value = value

    @classmethod
    def regex_matches(cls, text):
        return re.match(cls.regex, text)


class Integer(Node):
    regex = '^(\d+)$'


class Real(Node):
    regex = '^(\d+\.(\d+)?$|^(\d+)?\.\d+)$'


class Expression(Node):
    regex = '^\((.*)\)$'


class Parser:
    pass


class NumberParser(Parser):
    hierarchy = [Real, Integer]

    def __init__(self, input):
        self.input = input

    def get_rule(self, input):
        for N in self.hierarchy:
            if N.regex_matches(input):
                return N(input)
        raise RuleNotFound(input)


# class Evaluator:
#     precedence = [Expression

#     def __init__(self):
#         pass
