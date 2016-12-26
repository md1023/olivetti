#!/usr/bin/env python3

import re
from collections import namedtuple

Token = namedtuple('Tuple', ['kind', 'value'])
terminals = dict(
    ADD = '\+',
    MUL = '\*',
    SUB = '-',
    DIV = '/',
    MOD = '%',
    ASG = '=',
    DOT = '\.',
    LPR = '\(',
    RPR = '\)',
    LTR = '[A-Za-z]',
    DGT = '[0-9]',
    END = '\$'
)


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
                token = Token(_type, character)
                
        if not token:
            raise ValueError('unknown token %s' % _type)


        self.previous_token = token
        return token

    def get_next_token(self):
        token = self._get_next_token()
        print("got token", token)
        return token

    def repeat(self):
        print('repeating', self.previous_token)
        self.repeat_same_token = True


# l = Lexer('1 + 2')
# try:
#     t = l.get_next_token()
#     print(t)
#     while t:
#         print(l.get_next_token())
# except StopIteration:
#     print('exhausted')

class Parser:
    def parse(self, input):
        self.lexer = Lexer(input)
        expression_value = self.number()
        token = self.lexer.get_next_token()
        if token.kind == 'END':
            return expression_value
        else:
            raise Exception('END expected')

    def expression(self):
        print('expression')
        component1 = self.factor()
        operators = ['ADD', 'SUB']
        token = self.lexer.get_next_token()
        while token.kind in operators:
            component2 = self.factor()
            if token.kind == 'ADD':
                component1 += component2
            else:
                component2 -= component2
            token = self.lexer.get_next_token()
        self.lexer.repeat()
        return component1

    def factor(self):
        print('factor')
        factor1 = self.float()
        operators = ['MUL', 'DIV']
        token = self.lexer.get_next_token()
        while token.kind in operators:
            factor2 = self.float()
            if token.kind == 'MUL':
                factor1 *= factor2
            else:
                factor1 /= factor2
            token = self.lexer.get_next_token()
        self.lexer.repeat()
        return factor1

    def float(self):
        print('float')
        token = self.lexer.get_next_token()
        if token.kind == 'LPR':
            value = self.expression()
            expected_rparen = self.lexer.get_next_token()
            if expected_rparen.kind != 'RPR':
                raise Exception('unbalanced parenthesis')
        elif token.kind == 'DGT':
            value = int(token.value)
        else:
            raise Exception('not a float')
        return value

    def number(self):
        print('number')
        token = self.lexer.get_next_token()
        if token.kind == 'DGT':
            value = self.integer()
        if token.kind == 'DOT':
            

    def integer(self):
        

# p = Parser()
# print(p.parse('(2 + 3) * 2'))
