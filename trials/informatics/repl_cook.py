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


class TermOld:
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
        value = None
        for SF in nonterminal.subfactors:
            token_type = getattr(SF, 'token_type')
            print('SF', nonterminal, SF, token_type, parser.token, parser.next_token)
            if parser._accept(token_type) or token_type == 'ANY':
                value = self.visit(SF)
                if value is not None:
                    self.value = value
                    break

        if value is None:
            raise SyntaxError('unexpected end of tokens')

    def get_value(self):
        return self.value

    def visit(self, SF):
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
        self.stack.append(SF)
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
        self.stack.append(SF)
        value = Parser(self.parser).value
        while self.parser._accept('MUL', 'DIV', 'MOD'):
            operation = self.parser.token.type
            print('MIUL', operation, value)
            # TODO precedence failure
            if operation == 'MUL':
                value *= Parser(self.parser).value
            elif operation == 'DIV':
                value /= Parser(self.parser).value
            elif operation == 'MOD':
                value %= Parser(self.parser).value
        return value

    def visit_Factor(self, SF):
        self.stack.append(SF)
        value = Parser(self.parser).value
        return value

    def visit_Expression(self, SF):
        self.stack.append(SF)
        value = Parser(self.parser).value
        while self.parser._accept('ADD', 'SUB'):
            operation = self.parser.token.type
            if operation == 'ADD':
                value += Parser(self.parser).value
            elif operation == 'SUB':
                value -= Parser(self.parser).value
        return value


class NonTerminal:
    def __repr__(self):
        s = type(self).__name__
        s += '[{0}]'.format(getattr(self, 'token_type', ''))
        s += '{0}'.format([C.__name__ for C in getattr(self, 'subfactors', '')])
        return s


class Number:
    token_type = 'DGT'


class Identifier(NonTerminal):
    pass


class ParenExpression(NonTerminal):
    token_type = 'LPR'


class Add(NonTerminal):
    token_type = 'ADD'
    subfactors = 'Term'

            
class Sub(NonTerminal):
    token_type = 'SUB'
    subfactors = 'Factor'

            
class Mul(NonTerminal):
    token_type = 'MUL'
    subfactors = 'Factor'

            
class Div(NonTerminal):
    token_type = 'DIV'
    subfactors = 'Factor'


class Mod(NonTerminal):
    token_type = 'MOD'
    subfactors = 'Factor'
            

class Expression(NonTerminal):
    subfactors = 'Term'


class Term(NonTerminal):
    subfactors = 'Factor'


class Factor(NonTerminal):
    subfactors = 'Number Variable ParenExpression'


class Assignment(NonTerminal):
    token_type = 'ASG'
    subfactors = 'Expression'


class Variable(NonTerminal):
    token_type = 'VAR'
    subfactors = 'Assignment Identifier'
    def __init__(self, name):
        super().__init__()
        self.value = name


for C in NonTerminal.__subclasses__():
    if not hasattr(C, 'token_type'):
        setattr(C, 'token_type', 'ANY')

    if hasattr(C, 'subfactors'):
        subfactor_names = getattr(C, 'subfactors').split()
        setattr(C, 'subfactors', [])
        C.subfactors = [globals()[name] for name in subfactor_names]

e = Interpreter()
print(e.parse('ab_c = 5 + 545 - 2.5 * 2'))
# print(e.parse('ab_c = 2.8'))
print('memory', e.memory, '\n')
# print(e.parse('ab_c - 483'))
# print(e.parse('ab_c'))
