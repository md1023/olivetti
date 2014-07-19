#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from cmd import Cmd


class State:
    def __call__(self, state_cls, action_cls=None):
        self.__class__ = state_cls
        if action_cls in self.ACTIONS:
            pass

class Q1(State):
    ACTIONS = [S12]


class Q2(State):
    ACTIONS = [S23, S21]


class Q3(State):
    ACTIONS = [S34, S31]


class Q4(State):
    ACTIONS = [S43, S41]


class Device(State):
    def __init__(self):
        self(Q1)

    def connect(self):
        self(Q2, S12)

    def disconnect(self):
        self(Q1)

    def hold(self):
        self(Q4, S34)

    def unhold(self):
        self.(Q3, S43)


def msg(message):
    print(message)


class Prompt(Cmd):
    def __init__(self, device):
        super().__init__()
        self.device = device
        self.prompt = "(Phone) "
        self.cmdloop("(Phone) Type help for commands")

    def help_help(self):
        msg("Use the phone")

    def help_quit(self):
        msg("Stop execution")

    def do_quit(self, message):
        return True

    def do_connect(self, text):
        self.device.connect()

    def do_disconnect(self, text):
        self.device.connect()

    def do_hold(self, text):
        self.device.hold()

    def do_unhold(self, text):
        self.device.unhold()


d = Device()
d.connect()
Prompt(d)
