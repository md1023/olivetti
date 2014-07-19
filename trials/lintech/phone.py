#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from cmd import Cmd
        

# class Ready(State):
#     def 

# class Device(State):
#     def __init__(self):
#         self.buttons = 
    

class Button:
    READY = 0
    PUSHED = 1
    STUCK = 2

    def __init__(self, device):
        self.device = device
        self._state = self.READY

    def push(self):
        self._state = self.PUSHED
        self.action()
        self._state = self.READY

    def action(self):
        raise NotImplementedError()



class ConnectButton(Button):
    def action(self):
        self.device("S12")

class DisconnectButton(Button):
    def action(self):
        self.device("S41")


class State:
    def __call__(self, state_cls):
        self.__class__ = state_cls

class Connection(State):
    CONNECT = 0
    DISCONNECT = 0
    HOLD = 0
    UNHOLD = 0

class S12(Connection):
    pass

s12 = S12()
print(s12.CONNECT)

class Q1(State):
    ACTIONS = [S12]

class Q2(State):
    ACTIONS = [S23, S21]

class Q3(State):
    ACTIONS = [S34, S31]

class Q4(State):
    ACTIONS = [S43, S41]
    

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

class Device():
    def connect(self):
        pass

    def disconnect(self):
        pass

    def hold(self):
        pass

    def unhold(self):
        pass

d = Device()
d.connect()
Prompt(d)
