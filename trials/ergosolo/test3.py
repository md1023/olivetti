#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sqlite3

users = [(u"Платон Щукин",), (u"Лера Страза",), (u"Георгий Атласов",)]
message = [(1, u"Привет, Платон"),
           (3, u"Срочно пришли карту."),
           (3, u"Жду на углу Невского и Тверской."),
           (1, u"Это снова я, пиши чаще")]

db = sqlite3.connect('chat.db')
cursor = db.cursor()
cursor.executemany("insert into users (name) values (?)", users)
db.commit()
db.close()
