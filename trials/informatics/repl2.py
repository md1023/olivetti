#!/usr/bin/env python
import re
from collections import namedtuple

def tokenize(expression):
    if expression == "":
        return []

    regex = re.compile("\s*(=>|[-+*\/\%=\(\)]|[A-Za-z_][A-Za-z0-9_]*|[0-9]*\.?[0-9]+)\s*")
    tokens = regex.findall(expression)
    return [s for s in tokens if not s.isspace()]

def lexer(expression):
    for t in tokenize(expression):
        for _type, regexp in terminals.iteritems():
            if re.match(regexp, t):
                yield Token(t, _type)

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

# expression      ::= factor | expression operator expression
# factor          ::= number | identifier | assignment | '(' expression ')'
# assignment      ::= identifier '=' expression
# operator        ::= '+' | '-' | '*' | '/' | '%'
# identifier      ::= letter | '_' { identifier-char }
# identifier-char ::= '_' | letter | digit
# number          ::= { digit } [ '.' digit { digit } ]
grammar = dict(
    expression = ['factor', 'expression operator expression'],
    factor = ['number', 'identifier', 'assignment', 'LPR expression RPR'],
    assignment = ['identifier ASG expression'],
    operator = ['ADD', 'MUL', 'SUB', 'DIV', 'MOD'],
    identifier = ['LTR', '_ identifier_char'],
    identifier_char = ['_', 'LTR', 'DGT'],
    number = ['integer', 'integer . integer'],
    integer = ['DGT', 'integer DGT']
)

Token = namedtuple('Token', ['type', 'value'])

def is_terminal(symbol):
    if symbol in terminals:
        return True
    return False

class Interpreter:
    def __init__(self):
        self.vars = {}
        self.functions = {}

    def input(self, expression):
        tokens = list(lexer(expression))
        for t in tokens:
            print t
        stack = ['expression']
        expression = expression[::-1]
        for t in tokens:
            if is_terminal(stack[-1]) and stack[-1] == expression[-1]:
                stack.pop()
                print "eating", expression.pop()
            elif is_terminal(stack[-1]) and stack[-1] != expression[-1]:
                raise SyntaxError('expected %s' % stack[-1])
            for rule in grammar[stack[-1]]:
                
            

interpreter = Interpreter();
    
# Basic arithmetic
assert(interpreter.input("1 + 1 + b") == 2)
assert(interpreter.input("1 + 1") == 2)
assert(interpreter.input("2 - 1") == 1)
assert(interpreter.input("2 * 3") == 6)
assert(interpreter.input("8 / 4") == 2)
assert(interpreter.input("7 % 4") == 3)

# Variables
assert(interpreter.input("x = 1") == 1)
assert(interpreter.input("x") == 1)
assert(interpreter.input("x + 3") == 4)
# test.expect_error("input: 'y'", lambda : interpreter.input("y"))
