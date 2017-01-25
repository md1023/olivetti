#!/usr/bin/env python3

import re

from collections import namedtuple, OrderedDict

Token = namedtuple('Token', ['type', 'value'])

def tokenize(expression):
    if expression == "":
        return []

    regex = re.compile("\s*(=>|[-+*\/\%=\(\)]|[A-Za-z_][A-Za-z0-9_]*|[0-9]*\.?[0-9]+)\s*")
    tokens = regex.findall(expression)
    return [s for s in tokens if not s.isspace()]

terminals = OrderedDict((
    ('ADD', '\+'),
    ('MUL', '\*'),
    ('SUB', '-'),
    ('DIV', '/'),
    ('MOD', '%'),
    ('ASG', '=$'),
    ('DOT', '\.'),
    ('LPR', '\('),
    ('RPR', '\)'),
    ('FKW', '^fn$'),
    ('VAR', '[A-Za-z_]'),
    ('DGT', '[0-9]'),
    ('FOP', '^=>$')
))


class Token(object):
    __slots__ = 'type', 'value'
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __repr__(self):
        return "<{0} '{1}'>".format(self.type, self.value)


def lexer(expression):
    for t in tokenize(expression):
        for type, regexp in terminals.items():
            if re.match(regexp, t):
                yield Token(type, t)
                break


class Visitor:
    def visit(self, SF):
        visit_method = getattr(self, 'visit_' + SF.__name__)
        value = visit_method(SF)
        return value

    def visit_Number(self, SF):
        value = float(self.token.value)
        self._reject('DGT', 'VAR')
        return value

    def visit_Variable(self, SF):
        self.stack.append(SF(self.token.value))
        value = self()
        return value

    def visit_ParenExpression(self, SF):
        value = self.visit(Expression)
        self._expect('RPR')
        return value

    def visit_Assignment(self, SF):
        variable_name = self.stack[-1].value
        self.stack.append(SF)
        value = self()
        self.variables[variable_name] = value
        return value

    def visit_Identifier(self, SF):
        variable_name = self.stack[-1].value
        if variable_name not in self.variables:
            raise ValueError('undefined variable')
        value = self.variables[variable_name]
        self._reject('DGT', 'VAR')
        return value

    def visit_Term(self, SF):
        self.stack.append(SF)
        value = self()
        while self._accept('MUL', 'DIV', 'MOD'):
            self.stack.append(SF)
            operation = self.token.type
            if operation == 'MUL':
                value *= self()
            elif operation == 'DIV':
                value /= self()
            elif operation == 'MOD':
                value %= self()
        return value

    def visit_Factor(self, SF):
        self.stack.append(SF)
        value = self()
        return value

    def visit_FunctionDefinition(self, SF):
        self.stack.append(SF)
        value = None
        arguments = []
        while self._accept('VAR'):
            name = self.token.value
            arguments.append(name)
        self._expect('FOP')
        # print(arguments, self.token, self.next_token)
        expression_tokens = [self.next_token] + [t for t in self.tokens]

        undefined_arguments = [
            t.value
            for t in expression_tokens
            if t.type == 'VAR'
            if t.value not in arguments
        ]
        if undefined_arguments:
            raise SyntaxError('undefined arguments: {0}'.format(undefined_arguments))


        # self.stack.append(Expression)
        # value = self.visit(Expression)
        # print(value)
        print(expression_tokens)
        raise NotImplementedError('stop')
        return value

    def visit_Expression(self, SF):
        self.stack.append(SF)
        value = self()
        while self._accept('ADD', 'SUB'):
            self.stack.append(SF)
            operation = self.token.type
            if operation == 'ADD':
                value += self()
            elif operation == 'SUB':
                value -= self()
        return value


class Interpreter(Visitor):
    def __init__(self):
        self.variables = dict()

    def __call__(self):
        nonterminal = self.stack[-1]
        value = None
        for SF in nonterminal.subfactors:
            token_type = getattr(SF, 'token_type')
            print(SF, token_type, self.token, self.next_token)
            if self._accept(token_type) or token_type == 'ANY':
                value = self.visit(SF)
                if value is not None:
                    self.value = value
                    break

        if value is None:
            raise SyntaxError('unexpected end of tokens')

        return self.value

    def input(self, text):
        return self.parse(text)

    def parse(self, text):
        if not text.replace(' ', ''):
            return ''
        self.tokens = lexer(text)
        self.token = None
        self.next_token = None
        self._advance()
        self.stack = []
        return self.visit(Expression)

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


class Representable:
    def __repr__(self):
        s = type(self).__name__
        s += '[{0}]'.format(getattr(self, 'token_type', ''))
        s += '{0}'.format([C.__name__ for C in getattr(self, 'subfactors', '')])
        return s


class Terminal(Representable):
    pass


class NonTerminal(Representable):
    pass


class Number:
    token_type = 'DGT'


class Identifier(NonTerminal):
    pass


class ParenExpression(NonTerminal):
    token_type = 'LPR'


class Expression(NonTerminal):
    subfactors = 'Term'


class Term(NonTerminal):
    subfactors = 'Factor'


class FunctionDefinition(NonTerminal):
    token_type = "FKW"

class Factor(NonTerminal):
    subfactors = 'ParenExpression FunctionDefinition Number Variable'


class Assignment(NonTerminal):
    token_type = 'ASG'
    subfactors = 'Expression'


class Variable(Terminal):
    token_type = 'VAR'
    subfactors = 'Assignment Identifier'
    def __init__(self, name):
        super().__init__()
        self.value = name


for C in NonTerminal.__subclasses__() + Terminal.__subclasses__():
    if not hasattr(C, 'token_type'):
        setattr(C, 'token_type', 'ANY')

    if hasattr(C, 'subfactors'):
        subfactor_names = getattr(C, 'subfactors').split()
        setattr(C, 'subfactors', [])
        C.subfactors = [globals()[name] for name in subfactor_names]

e = Interpreter()
# print(e.parse('fn foo bar => 1 + foo + bar'))
# print(e.parse('fn ager fnhtr gaer'))
# print(e.parse('1 + 2'))
print(e.parse('ab_c = 5 '))
# print(e.parse('(1+2)'))
print(e.parse('ab_c * ab_c'))
# print('variables', e.variables)
# print(e.parse('ab_c - 483'))
# print(e.parse('ab_c'))
