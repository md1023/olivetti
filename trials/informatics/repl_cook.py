#!/usr/bin/env python3

import re
import itertools

from collections import namedtuple, OrderedDict

Token = namedtuple('Token', ['type', 'value'])

OFFSET=''

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
        return value

    def visit_Variable(self, SF):
        # self.stack.append(SF(self.token.value))
        self.stack.append(SF)
        value = self()
        return value

    def visit_ParenExpression(self, SF):
        value = self.visit(Expression)
        self._expect('RPR')
        return value

    def visit_Assignment(self, SF):
        name = self.stack[-1].value
        print('ass', name, self.token, self.next_token)
        if name in self.memory and isinstance(self.memory[name], Function):
            raise IndexError('Cannot overwrite function variable \'{0}\''.format(name))
        self.stack.append(SF)
        value = self()
        self.memory[name] = value
        return value

    def visit_FunctionCall(self, SF):
        global OFFSET
        OFFSET += '    '
        # func_name = self.token.value
        # func = self.memory[self.token.value]
        func = self.stack[-1]
        print(OFFSET + 'FUNCTIONCALL REACHED', func.name, len(func.arguments), 'TOKENS:', self.token, self.next_token)

        # if not isinstance(func, Function):
        #     return None
        # get variable values
        memory = dict()
        for var in func.arguments:
            # self._advance()
            self.stack.append(Expression)
            print(OFFSET + 'VAR', var, self.token, self.next_token)
            value = self()
            print(OFFSET + 'OUTVAR', value, self.token, self.next_token)
            memory[var] = value
        # raise NotImplementedError('stop')

        # self._reject('DGT')

        # substitute variables into function body
        print(OFFSET + 'NEW PARSER', func.name, func.body)
        e = Interpreter()
        e.tokens = iter(func.body)
        e.memory = memory
        value = e.start()
        print(OFFSET + 'PARSER FINISHED', value, func.name, memory, self.token, self.next_token)
        OFFSET = OFFSET[:-4]
        return value

    def visit_Identifier(self, SF):
        name = self.token.value
        if name not in self.memory:
            raise ValueError('undefined variable \'{0}\' {1}'.format(name, self.memory))
        value = self.memory[name]

        # value is number
        if not isinstance(value, Function):
            return value

        # value is not a number
        self.stack.append(value)
        value = self.visit(FunctionCall)
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
        # self._reject('DGT', 'VAR')
        # if self._accept('DGT', 'VAR'):
        #     return None
        if self.next_token and self.next_token.type == 'DGT':
            return None
        if self.next_token and self.next_token.type == 'VAR':
            return None
        return value

    def visit_Function(self, SF):
        arguments = list()
        
        # get function name
        self._expect('VAR')
        name = self.token.value

        # get function arguments
        while self._accept('VAR'):
            var_name = name + '__' + self.token.value
            if var_name in arguments:
                raise SyntaxError('Duplicate variable name \'{0}\''.format(self.token.value))
            arguments.append(var_name)
            print('vars', arguments)
        self._expect('FOP')

        # get function body
        # TODO awkward for maybe better?
        body = []
        for t in itertools.chain([self.next_token], self.tokens):
            if t.type != 'VAR':
                body.append(t)
                continue
            old_name = t.value
            t.value = name + '__' + old_name
            if t.value not in arguments:
                raise SyntaxError('undefined arguments: {0}'.format(old_name))
            body.append(t)
        print('BOD', body)

        # save function instance into memory
        if name in self.memory and not isinstance(self.memory[name], SF):
            raise IndexError('Cannot overwrite non function variable \'{0}\''.format(name))
        self.memory[name] = SF(name, arguments, body)

        # self.tokens was exhausted
        # TODO self.token and self.next_token should be attached to iterator object
        self.token = self.next_token = None
        
        # return reference to nameless function instance
        return '' # self.memory[name]

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
        self.memory = dict()
        self.token = None
        self.next_token = None
        self.stack = []

    def __call__(self):
        global OFFSET
        nonterminal = self.stack[-1]
        value = None
        for SF in nonterminal.subfactors:
            token_type = getattr(SF, 'token_type')
            print('{0}{1}'.format(OFFSET, SF), token_type, self.token, self.next_token)
            if self._accept(token_type) or token_type == 'ANY':
                value = self.visit(SF)
                if value is not None:
                    self.value = value
                    break

        # if value is None:
        #     raise SyntaxError('unexpected end of tokens')

        return self.value

    def start(self):
        self._advance()
        return self.visit(Expression)

    def parse(self, text):
        if not text.replace(' ', ''):
            return ''
        print(text)
        self.tokens = lexer(text)
        self.token = None
        self.next_token = None
        self.stack = []
        return self.start()

    def _advance(self):
        print('{0}ATE {1}'.format(OFFSET, self.token))
        # from pdb import set_trace; set_trace()
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
    subfactors = 'FunctionCall'


class FunctionCall(NonTerminal):
    token_type = 'VAR'


class ParenExpression(NonTerminal):
    token_type = 'LPR'


class Expression(NonTerminal):
    subfactors = 'Term'


class Term(NonTerminal):
    subfactors = 'Factor'


class Function(NonTerminal):
    token_type = 'FKW'
    subfactors = 'FunctionCall'
    def __init__(self, name, arguments, body):
        self.name = name
        self.arguments = arguments
        self.body = body

    def __repr__(self):
        s = type(self).__name__
        s += '[{0}({1})]'.format(self.name, self.arguments)
        return s

        
class Factor(NonTerminal):
    # TODO Function may be moved to variable
    subfactors = 'ParenExpression Function Number Variable'


class Assignment(NonTerminal):
    token_type = 'ASG'
    subfactors = 'Expression'


class Variable(Terminal):
    token_type = 'VAR'
    subfactors = 'Assignment Identifier'
    # def __init__(self, name):
    #     self.value = name


for C in NonTerminal.__subclasses__() + Terminal.__subclasses__():
    if not hasattr(C, 'token_type'):
        setattr(C, 'token_type', 'ANY')

    if hasattr(C, 'subfactors'):
        subfactor_names = getattr(C, 'subfactors').split()
        setattr(C, 'subfactors', [])
        C.subfactors = [globals()[name] for name in subfactor_names]

e = Interpreter()
# # e.memory['foo'] = list()
# # print(e.parse('foo = 5 '), '\n')

# print(e.parse('fn foo bar => bar + 1'))
# print(e.parse('foo 2'))

print(e.parse('fn avg x y => (x + y) / 2'))
# # print(e.parse('avg 4 2'))
# # print(e.parse('avg 7 2 4')) # wrong
# # print(e.parse('avg = 5')) # wrong
print(e.parse('fn echo var => var'))
print(e.parse('avg echo 2 echo 4'))
# print(e.parse('echo 1'))

# print(e.parse('fn one => 1'))
# print(e.parse('1 2')) # wrong
# print(e.parse('1two')) # wrong

# print(e.parse('fn add x x => x + x'))

# # print(e.parse('fn ager fnhtr gaer'))
# # print(e.parse('1 + 2'))

# # print(e.parse('ab_c = 5 '))
# # print(e.parse('(1+2)'))
# # print(e.parse('ab_c * ab_c'))

# # print(e.parse('ab_c - 483'))
# # print(e.parse('ab_c'))
