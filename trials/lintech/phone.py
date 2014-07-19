#!/usr/bin/env python3
# -*- coding: utf-8 -*-

class Device:
    READY = 0
    ESTABLISH = 1
    CALL = 2
    HOLD = 3

    def __init__(self):
        self.state = READY


class Button:
    READY = 0
    PUSHED = 1
    STUCK = 2

    def __init__(self):
        self.__state__ = self.READY

    def push(self):
        self.state = self.PUSHED
        self.state = self.READY

    @property
    def state(self, value):
        self.__state = value
        print(value)

b = Button()
b.push()
