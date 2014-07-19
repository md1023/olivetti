#!/usr/bin/env python3
# -*- coding: utf-8 -*-


        

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

class Q1(State):
    ACTIONS = [S12]

class Q2(State):
    ACTIONS = [S23, S21]

class Q3(State):
    ACTIONS = [S34, S31]

class Q4(State):
    ACTIONS = [S43, S41]
    

class Device():
    def connect():
        pass

    def disconnect():
        pass

    def hold():
        pass

    def unhold():
        pass

d = Device()
d.connect()

class Connection(State):
    CONNECT = 0
    DISCONNECT = 0
    HOLD = 0
    UNHOLD = 0

class S12(Connection):
    pass

class S

s12 = S12()
print(s12.CONNECT)
