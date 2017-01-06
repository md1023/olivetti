#!/usr/bin/env python3

import re

from collections import namedtuple

Token = namedtuple('Token', ['type', 'value'])

def tokenize(expression):
    if expression == "":
        return []

    regex = re.compile("\s*(=>|[-+*\/\%=\(\)]|[A-Za-z_][A-Za-z0-9_]*|[0-9]*\.?[0-9]+)\s*")
    tokens = regex.findall(expression)
    return [s for s in tokens if not s.isspace()]

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
    VAR = '[A-Za-z]',
    DGT = '[0-9]'
)

def lexer(expression):
    for t in tokenize(expression):
        for _type, regexp in terminals.items():
            if re.match(regexp, t):
                yield Token(_type, t)


class Interpreter:
    def __init__(self):
        self.memory = dict()

    def input(self, text):
        return self.parse(text)

    def parse(self, text):
        print('test', text)
        if not text.replace(' ', ''):
            return ''
        self.tokens = lexer(text)
        print(list(lexer(text)))
        self.token = None
        self.next_token = None
        self._advance()
        res = Expression(self).value
        print('result', res)
        return res

    def _advance(self):
        self.token, self.next_token = self.next_token, next(self.tokens, None)

    def _accept(self, *token_types):
        for token_type in token_types:
            if self.next_token and self.next_token.type == token_type:
                self._advance()
                return True
        return False

    def _expect(self, token_type):
        if not self._accept(token_type):
            raise SyntaxError('Expected {0}'.format(token_type))

    def _reject(self, *token_types):
        if any(self._accept(token_type) for token_type in token_types):
            raise SyntaxError('unexpected {0}'.format(token_type))


class Expression:
    'expression ::= term { ("+"|"-") term }*'
    def __init__(self, parser):
        expression_value = Term(parser).value
        while parser._accept('ADD') or parser._accept('SUB'):
            operation = parser.token.type
            right = Term(parser).value
            print('expr', right, operation, expression_value)
            if operation == 'ADD':
                expression_value += right
            elif operation == 'SUB':
                expression_value -= right
        self.value = expression_value

    
class Term:
    'term ::= factor { ("*"|"/") factor }*'
    def __init__(self, parser):
        self.value = Factor(parser).value
        while parser._accept('MUL', 'DIV', 'MOD'):
            operation = parser.token.type
            right = Factor(parser).value
            if operation == 'MUL':
                self.value *= right
            elif operation == 'DIV':
                self.value /= right
            elif operation == 'MOD':
                self.value %= right
        
        
class Factor:
    'factor ::= digit | identifier | assignment | ( expr )'
    def __init__(self, parser):
        if parser._accept('DGT'):
            value = float(parser.token.value)
            parser._reject('DGT', 'VAR')
            self.value = value
        elif parser._accept('VAR'):
            variable_name = parser.token.value
            if parser._accept('ASG'):
                self.value = Assignment(parser, variable_name).value
            else:
                self.value = Identifier(parser).value
        elif parser._accept('LPR'):
            expression_value = Expression(parser).value
            parser._expect('RPR')
            self.value = expression_value
        else:
            raise SyntaxError('expected DGT or LPR')

        
class Assignment:
    def __init__(self, parser, variable_name):
        self.value = Expression(parser).value
        parser.memory[variable_name] = self.value

        
class Identifier:
    def __init__(self, parser):
        variable_name = parser.token.value
        if variable_name not in parser.memory:
            raise ValueError('undefined variable')
        self.value = parser.memory[variable_name]

    
e = Interpreter()
print(e.parse('ab_c = 2.8+543.'))
print('memory', e.memory)
print(e.parse('ab_c - 483'))
# print(e.parse('ab_c'))
