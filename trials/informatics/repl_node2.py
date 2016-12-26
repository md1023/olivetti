#!/usr/bin/env python3
import re
from collections import namedtuple

Token = namedtuple('Tuple', ['kind', 'value'])

class Lexer:
    def __init__(self, input):
        self.input = iter(input.replace(' ', '') + '$')
        self.repeat_same_token = False

    def _get_next_token(self):
        if self.repeat_same_token:
            self.repeat_same_token = False
            return self.previous_token

        character = next(self.input)
        token = None
        
        for _type, regexp in terminals.items():
            if re.match(regexp, character):
                if isinstance(_type, str):
                    # TODO drive away namedtuples
                    token = Token(_type, character)
                else:
                    token = _type(character)
                
        if not token:
            raise ValueError('unknown token %s' % _type)


        self.previous_token = token
        return token

    def get_next_token(self):
        token = self._get_next_token()
        # print("got token", token)
        return token

    def repeat(self):
        # print('repeating', self.previous_token)
        self.repeat_same_token = True

        
class ParserError(Exception):
    pass


class Node:
    pass


class Terminal(Node):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return "{0}[{1}]".format(self.__class__.__name__, self.value)
        
class Digit(Terminal):
    pass


class Dot(Terminal):
    pass


class End(Terminal):
    pass


terminals = {
    'ADD': '\+',
    'MUL': '\*',
    'SUB': '-',
    'DIV': '/',
    'MOD': '%',
    'ASG': '=',
    Dot: '\.',
    'LPR': '\(',
    'RPR': '\)',
    'LTR': '[A-Za-z]',
    Digit: '[0-9]',
    End: '\$'
}

class NonTerminal(Node):
    pass


class Number(NonTerminal):
    pass


class Integer(NonTerminal):
    pass


def match(token, first):
    return isinstance(token, first)


l = Lexer('3..5')

rule_map = {
    (Number, Dot): [Dot, Integer],
    (Number, Digit): [Digit, Number],
    (Integer, Digit): [Digit, Integer],
    (Integer, End): [End]
}

step = 0
value = ''
stack = [Number]
token = l.get_next_token()


while token:
    first = stack.pop(0)
    print(token, first, match(token, first), (first, token.__class__) in rule_map)
    if match(token, first):
        value += token.value
        token = l.get_next_token()
    elif (first, token.__class__) in rule_map:
        rule = rule_map[(first, token.__class__)]
        stack = rule + stack
    else:
        raise ParserError()
    print('{0})'.format(step), stack, token, '[{0}]'.format(value),'\n')
    step += 1
