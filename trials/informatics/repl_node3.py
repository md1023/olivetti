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
        self.tokens = self.tokenizer(input)

    def tokenizer(self, characters):
        for c in characters:
            token = next(
                (T(c) for T in self.map if re.match(T.regex, c)),
                None)
            if not token:
                raise TokenNotFound('unknown character \'{0}\''.format(c))
            yield token


class Node(Printable):
    def __init__(self, value, subnodes=None):
        if subnodes:
            self.subnodes = subnodes
        self.value = value


class Integer(Node):
    regex = '^\d+$'


class Real(Node):
    regex = '^\d+\.(\d+)?$|^(\d+)?\.\d+$'


class Parser:
    hierarchy = [Real, Integer]

    def __init__(self, input):
        pass
        self.input = input

    def get_rule(self, input):
        for N in self.hierarchy:
            if self.match_rule(N, input):
                return N(input)
        raise RuleNotFound(input)


    def match_rule(self, node, text):
        return re.match(node.regex, text)
