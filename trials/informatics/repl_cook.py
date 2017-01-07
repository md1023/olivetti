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

    def _expect(self, *token_types):
        if not all(self._accept(token_type) for token_type in token_types):
            raise SyntaxError('Expected {0}'.format(token_types))

    def _reject(self, *token_types):
        if any(self._accept(token_type) for token_type in token_types):
            raise SyntaxError('unexpected {0}'.format(token_types))


class Expression:
    'expression ::= term { ("+"|"-") term }*'
    def __init__(self, parser):
        expression_value = Term(parser).value
        while parser._accept('ADD', 'SUB'):
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


class FactorMixin:
    def next_factor(self, parser, variable_name=None):
        for SF in self.subfactors:
            value = SF(parser, variable_name).value
            if value is not None:
                return value


class Factor(FactorMixin):
    'factor ::= digit | variable | ( expr )'
    def __init__(self, parser):
        self.value = self.next_factor(parser)
        if self.value is None:
            raise SyntaxError('expected DGT or LPR')


class FactorSub:
    def __init__(self, parser, variable_name=None):
        self.value = None
        if parser._accept(self.token_type):
            if variable_name is None:
                self.visit(parser)
            else:
                self.visit(parser, variable_name)


class FactorDigit(FactorSub):
    token_type = 'DGT'
    def visit(self, parser):
        value = float(parser.token.value)
        parser._reject('DGT', 'VAR')
        self.value = value


class FactorVariable(FactorSub, FactorMixin):
    token_type = 'VAR'
    def visit(self, parser):
        self.value = self.next_factor(parser, parser.token.value)
        if self.value is None:
            raise NotImplementedError('fucking error')


class FactorExpression(FactorSub):
    token_type = 'LPR'
    def visit(self, parser):
        expression_value = Expression(parser).value
        parser._expect('RPR')
        self.value = expression_value


class Assignment(FactorSub):
    token_type = 'ASG'
    def visit(self, parser, variable_name):
        self.value = Expression(parser).value
        parser.memory[variable_name] = self.value


# doesn't inherit FactorSub, otherwise token will be eaten
class Identifier:
    def __init__(self, parser, variable_name):
        if variable_name not in parser.memory:
            raise ValueError('undefined variable')
        self.value = parser.memory[variable_name]


Factor.subfactors = [FactorDigit, FactorVariable, FactorExpression]
FactorVariable.subfactors = [Assignment, Identifier]

e = Interpreter()
print(e.parse('ab_c = 2.8+543.'))
print('memory', e.memory, '\n')
print(e.parse('ab_c - 483'))
# print(e.parse('ab_c'))
