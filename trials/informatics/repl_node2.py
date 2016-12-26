#!/usr/bin/env python3
import re
from collections import namedtuple

Token = namedtuple('Tuple', ['kind', 'value'])
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
    DGT = '[0-9]',
    END = '\$'
)

class Lexer:
    def __init__(self, input):
        self.input = iter(input.replace(' ', '') + '$')
        self.repeat_same_token = False

    def _get_next_token(self):
        if self.repeat_same_token:
            self.repeat_same_token = False
            return self.previous_token

        character = next(self.input)
        token = None
        
        for _type, regexp in terminals.items():
            if re.match(regexp, character):
                token = Token(_type, character)
                
        if not token:
            raise ValueError('unknown token %s' % _type)


        self.previous_token = token
        return token

    def get_next_token(self):
        token = self._get_next_token()
        print("got token", token)
        return token

    def repeat(self):
        print('repeating', self.previous_token)
        self.repeat_same_token = True

class ParserError(Exception):
    pass


class Parser:
    def parse(self, input):
        self.lexer = Lexer(input)
        self.stack = [Number]
        # token = self.lexer.get_next_token()

        node = self.stack.pop()(Integer, Integer)
        
        self.stack = getattr(self, 'visit' + type(node).__name__)(node)
        print(self.stack)
        

    def visit(self, node):
        print(node, node.__name__, self.stack)
        new_node = getattr(self, 'visit' + node.__name__)(node)
        print("bbb", new_node)

        
    def visitDigit(self, token):
        """
        Digit(1)
        """
        number = int(token.value)
        return number

    
    def visitInteger(self, node):
        """
        Integer(Digit(1), Digit(2))
        """
        token = self.lexer.get_next_token()
        digits = []
        while token.kind is 'DGT':
            print('while', token)
            digits.append(self.visitDigit(token))
            token = self.lexer.get_next_token()
        self.lexer.repeat()
        print('gaer', digits)
        if digits:
            number = "".join(str(d) for d in digits)
            return int(number)
        raise ParserError()

    
    def visitNumber(self, node):
        """
        Number(Integer(Digit(1), Digit(2)), Integer(Digit(3)))
        """
        integer1 = self.visitInteger(node)
        token = self.lexer.get_next_token()
        if token.kind is 'DOT':
            # eat dot
            token = self.lexer.get_next_token()
        else:
            self.lexer.repeat()
            return int(integer1)
        integer2 = self.visitInteger(node)
        number = "{0}.{1}".format(integer1, integer2)
        return float(number)

class Node:
    pass


class Terminal(Node):
    def __init__(self, value):
        self.value = value

        
class Digit(Terminal):
    pass


class Dot(Terminal):
    pass


class NonTerminal(Node):
    pass


class Number(NonTerminal):
    def __init__(self, integral, fraction):
        self.integral = integral
        self.fraction = fraction

class Integer(NonTerminal):
    def __init__(self, *digits):
        if not digits:
            digits = []
        self.digits = digits


p = Parser()
p.parse('5.3')
