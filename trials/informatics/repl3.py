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
# Rule = namedtuple('Rule', ['token', ''])

def is_terminal(symbol):
    if symbol in terminals:
        return True
    return False


class Node:
    pass


class Terminal(Node):
    def __init__(self, character):
        self.character = character

        
class Operator(Terminal):
    pass


class Assignment(Terminal):
    pass


class Dot(Terminal):
    pass


class LeftParenthesis(Terminal):
    pass


class RightParenthesis(Terminal):
    pass


class Letter(Terminal):
    pass


class Digit(Terminal):
    pass


class NonTerminal(Node):
    pass


class Number(NonTerminal):
    # rules = iter([Integer, [Integer, Dot, Integer]])
    pass


class Integer(NonTerminal):
    # rules = iter([Digit, [Integer, Digit]])
    pass


class Parser():
    def visit_Number(self, node):
        yield [Integer]
        yield [Integer, Dot, Integer]

    def visit_Integer(self, node):
        yield [Digit]
        yield [Integer, Digit]

    def visit_Digit(self, node):
        return node.character
        

# class UnaryOperator(Node):
#     def __init__(self, operand):
#         self.operand

        
# class BinaryOperator(Node):
#     def __init__(self, left, right):
#         self.left = left
#         self.right = right




class Grammar 

    
class Interpreter:
    def __init__(self):
        self.vars = {}
        self.functions = {}

    def input(self, expression):
        tokens = list(lexer(expression))
        for t in tokens:
            print t
        # number for example
        # for r in self.generate_rule('number'):
        #     print "RULE:", r

    def generate_rule(self, rule):
        if not rule:
            raise Exception('no rule, you fool')
        stack = [rule]
        depth = 0
        while stack:
            depth += 1
            if depth > 20:
                raise Exception('recursion limit reached')
            rule, opt_rules = grammar.get(stack[-1])
            stack.extend(opt_rules.split())
            print depth, stack, rule, is_terminal(rule), '[', opt_rules, ']'
            if isinstance(rule, list):
                raise NotImplementedError('complex rule not implemented: %s' % rule)
            if not is_terminal(rule):
                stack.pop()
                stack.append(rule)
            if is_terminal(rule):
                stack.pop()
                yield rule

interpreter = Interpreter();
    
# Basic arithmetic
assert(interpreter.input("(1 + 1 + b)") == 2)
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
