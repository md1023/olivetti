#!/usr/bin/env python3

import re
from repl_node3 import Lexer


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
    LTR = '[A-Za-z]',
    DGT = '[0-9]'
)

def lexer(expression):
    for t in tokenize(expression):
        for _type, regexp in terminals.items():
            if re.match(regexp, t):
                yield Token(_type, t)

# l = list(lexer('a= 1+ 2'))
# print(l)


class ExpressionEvaluator:
    def parse(self, text):
        self.tokens = lexer(text)
        self.memory = dict()
        print(list(lexer(text)))
        self.token = None
        self.next_token = None
        self._advance()
        return self.expr()

    def _advance(self):
        self.token, self.next_token = self.next_token, next(self.tokens, None)

    def _accept(self, token_type):
        if self.next_token and self.next_token.type == token_type:
            print('_accept', self.next_token, self.next_token.type, token_type)
            self._advance()
            return True
        else:
            return False

    def _expect(self, token_type):
        if not self._accept(token_type):
            raise SyntaxError('Expected {0}'.format(token_type))


    def expr(self):
        'expression ::= term { ("+"|"-") term }*'
        expression_value = self.term()
        while self._accept('ADD') or self._accept('SUB'):
            operation = self.token.type
            right = self.term()
            if operation == 'ADD':
                expression_value += right
            elif operation == 'SUB':
                expression_value -= right
        return expression_value


    def term(self):
        'term ::= factor { ("*"|"/") factor }*'
        term_val = self.factor()
        while self._accept('MUL') or self._accept('DIV'):
            operation = self.token.type
            right = self.factor()
            if operation == 'MUL':
                term_val *= right
            elif operation == 'DIV':
                term_val /= right
        return term_val

    def factor(self):
        'factor ::= digit | identifier | assignment | ( expr )'
        if self._accept('DGT'):
            return float(self.token.value)
        elif self._accept('LTR') and self._accept('ASG'):
            self.assignment()
        elif self._accept('LTR'):
            self.identifier()
        elif self._accept('LPR'):
            expression_value = self.expr()
            self._expect('RPR')
            return expression_value
        else:
            raise SyntaxError('expected NUM or LPR')


    def assignment(self):
        print('fgareg')

    def identifier(self):
        variable_name = self.token.value
        if variable_name in self.memory:
            return self.memory[variable_name]
        raise ValueError('undefined variable')


    # RUBBISH
    # def number(self):
    #     'number ::= { digit }* { "." digit { digit }* }'
    #     print('number', self.token, self.next_token)
    #     raise NotImplemented()

e = ExpressionEvaluator()
# print(e.parse('ab_c = 2.8+543.'))
print(e.parse('ab_c'))
