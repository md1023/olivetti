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
        self.stack = [Expression]
        # res = Expression(self).value
        res = Parser(self).get_value()
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


class ExpressionOld:
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
    def __init__(self, parser):
        self.parser = parser
        self.stack = parser.stack
        nonterminal = self.stack[-1]
        print('NT', nonterminal)
        for SF in nonterminal.subfactors:
            token_type = getattr(SF, 'token_type')
            # print('SF', token_type, parser.token, parser.next_token)
            if parser._accept(token_type) or token_type == 'ANY':
                value = self.visit(SF)
                if value is not None:
                    self.value = value
                    break

        if value is None:
            raise SyntaxError('unexpected end of tokens')

    def get_value(self):
        return self.value

    def visit(self, SF, **kwargs):
        visit_method = getattr(self, 'visit_' + SF.__name__)
        print('visit', SF.__name__)
        return visit_method(SF)

    def visit_Number(self, SF):
        value = float(self.parser.token.value)
        self.parser._reject('DGT', 'VAR')
        return value

    def visit_Variable(self, SF):
        self.stack.append(SF(self.parser.token.value))
        value = Parser(self.parser).value
        return value

    def visit_ParenExpression(self, SF):
        expression_value = SF(self.parser).value
        self.parser._expect('RPR')
        return expression_value

    def visit_Assignment(self, SF):
        variable_name = self.stack[-1].value
        self.stack.append(Expression)
        value = Parser(self.parser).value
        self.parser.memory[variable_name] = value
        return value

    def visit_Identifier(self, SF):
        if variable_name not in self.parser.memory:
            raise ValueError('undefined variable')
        value = self.parser.memory[variable_name]
        self.parser._reject('DGT', 'VAR')
        return value

    def visit_Term(self, SF):
        self.stack.append(Factor)
        value = Parser(self.parser).value
        return value


class Number:
    token_type = 'DGT'
    # subfactors = [Number]


class Assignment:
    token_type = 'ASG'


class Identifier:
    token_type = 'ANY'


class Variable:
    token_type = 'VAR'
    subfactors = [Assignment, Identifier]
    def __init__(self, name):
        self.value = name


class ParenExpression:
    token_type = 'LPR'


class Factor:
    token_type = 'ANY'
    subfactors = [Number, Variable, ParenExpression]


class Term:
    token_type = 'ANY'


class Expression:
    token_type = 'ANY'
    subfactors = [Term]


e = Interpreter()
print(e.parse('ab_c = 2.8'))
# print(e.parse('ab_c = 2.8'))
print('memory', e.memory, '\n')
# print(e.parse('ab_c - 483'))
# print(e.parse('ab_c'))
