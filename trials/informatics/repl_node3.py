#!/usr/bin/env python3
import re


class Node:
    pass


class Terminal(Node):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return "{0}[{1}]".format(self.__class__.__name__, self.value)

    
class Digit(Terminal):
    regex = '[0-9]'


class Dot(Terminal):
    regex = '\.'


class End(Terminal):
    regex = '\$'


class TokenNotFound(Exception):
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

l = Lexer('1.2$')
print(list(l.tokens))
