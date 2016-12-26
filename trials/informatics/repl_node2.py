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

        character = next(self.input, None)
        if not character:
            return None
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


class Parser:
    def __init__(self, input):
        self.lexer = Lexer(input)
        

    def Number(self):
        print('\n\n')
        l = self.lexer
        rule_map = {
            (Number, Digit): [Digit, Number],
            (Number, Dot): [Dot, Integer],
            (Number, End): [],
            (Integer, Digit): [Digit, Integer],
            (Integer, End): []
        }        
        value = ''
        stack = [Number, End]
        t = l.get_next_token()
        tokens = []
        while t:
            tokens.append(t)
            t = l.get_next_token()
        print(tokens, stack)
        
        if len(tokens) == 1:
            raise ParserError()

        position = 0
        while len(stack) > 0:
            first = stack.pop(0)
            token = tokens[position]
            if issubclass(first, Terminal):
                if match(token, first):
                    position += 1
                    value += token.value
                    print('pop', value)
                    if match(token, End):
                        print('input accepted')
                else:
                    raise ParserError()
            elif issubclass(first, NonTerminal):
                # print('first', first, 'token', token)
                rule = rule_map[(first, token.__class__)]
                stack = rule + stack
            # print('stack', stack)
        return float(value[:-1])

# s = '342.5'
# p = Parser(s)
# n = p.Number()
# print(n)
# assert n, s
