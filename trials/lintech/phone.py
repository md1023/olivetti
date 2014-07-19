#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from cmd import Cmd


class Q1:
    # CONNECT BLOCK
    @staticmethod
    def S12(device):
        print("connecting", device)


class Q2:
    # ESTABLISH BLOCK
    @staticmethod
    def S23(device):
        pass

    # DISCONNECT BLOCK
    @staticmethod
    def S21(device):
        pass


class Q3:
    # HOLD BLOCK
    @staticmethod
    def S34(device):
        pass

    # DISCONNECT BLOCK
    @staticmethod
    def S31(device):
        pass


class Q4:
    # HOLD BLOCK
    @staticmethod
    def S43(device):
        pass

    # DISCONNECT BLOCK
    @staticmethod
    def S41(device):
        pass


class State:
    def __init__(self, state=None):
        self._state = state

    def __call__(self, state_cls, handler_name=None):
        handler = getattr(self._state, handler_name)
        if handler:
            handler(self)
        self._state = state_cls

class Device(State):
    def __init__(self):
        super().__init__(Q1)

    def connect(self):
        self(Q2, "S12")

    def disconnect(self):
        self(Q1)

    def hold(self):
        self(Q4, "S34")

    def unhold(self):
        self(Q3, "S43")


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
# Prompt(d)
