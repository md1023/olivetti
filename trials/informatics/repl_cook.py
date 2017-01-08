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
        print('TST', text)
        if not text.replace(' ', ''):
            return ''
        self.tokens = lexer(text)
        print(list(lexer(text)))
        self.token = None
        self.next_token = None
        self._advance()
        res = Expression(self).value
        print('RES', res)
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
    token_type = None
    'expression ::= term { ("+"|"-") term }*'
    def __init__(self, parser):
        expression_value = Term(parser).value
        while parser._accept('ADD', 'SUB'):
            operation = parser.token.type
            right = Term(parser).value
            print('EXP', right, operation, expression_value)
            if operation == 'ADD':
                expression_value += right
            elif operation == 'SUB':
                expression_value -= right
        self.value = expression_value


class Term:
    'term ::= factor { ("*"|"/") factor }*'
    def __init__(self, parser):
        self.value = Parser(parser, FactorObject()).value
        while parser._accept('MUL', 'DIV', 'MOD'):
            operation = parser.token.type
            right = Parser(parser, FactorObject()).value
            if operation == 'MUL':
                self.value *= right
            elif operation == 'DIV':
                self.value /= right
            elif operation == 'MOD':
                self.value %= right


class Parser:
    def __init__(self, parser, nonterminal):
        self.parser = parser
        for SF in nonterminal.subfactors:
            token_type = getattr(SF, 'token_type')
            if parser._accept(token_type) or token_type == 'ANY':
                value = self.visit(SF)
                if value is not None:
                    self.value = value

        raise SyntaxError('unexpected end of tokens')

    def visit(self, SF):
        visit_method = getattr(self, 'visit_' + SF.__name__)
        print('visit', SF.__name__)
        return visit_method(SF)

    def visit_Number(self, SF):
        value = float(self.parser.token.value)
        self.parser._reject('DGT', 'VAR')
        print('num', value)
        return value

    def visit_Variable(self, SF):
        assert SF.__name__ == 'Variable'
        variable = SF()
        variable.variable_name = self.parser.token.value
        value = Parser(self.parser, variable).value
        return value

    def visit_Expression(self, SF):
        expression_value = SF(self.parser).value
        self.parser._expect('RPR')
        return expression_value

    def visit_Assignment(self, SF, variable_name):
        value = Expression(self.parser).value
        self.parser.memory[variable_name] = value
        print('asg', value)
        return value

    def visit_Identifier(self, SF, variable_name):
        if variable_name not in self.parser.memory:
            raise ValueError('undefined variable')
        value = self.parser.memory[variable_name]
        self.parser._reject('DGT', 'VAR')
        return value


class Number:
    token_type = 'DGT'


class FactorObject:
    subfactors = [Number, Variable] #, Expression]


class VariableObject:
    token_type = 'VAR'
    subfactors = [Assignment, Identifier]


class Assignment:
    token_type = 'ASG'


class Identifier:
    token_type = 'ANY'


e = Interpreter()
print(e.parse('ab_c = 2.8+543.'))
print('memory', e.memory, '\n')
print(e.parse('ab_c - 483'))
# print(e.parse('ab_c'))
