#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sqlite3
from os.path import isfile

DB_NAME = "chat.db"
USERS = [(u"Платон Щукин",), (u"Лера Страза",), (u"Георгий Атласов",)]
MESSAGES = [(1, u"Привет, Платон"),
           (3, u"Срочно пришли карту."),
           (3, u"Жду на углу Невского и Тверской."),
           (1, u"Это снова я, пиши чаще")]

if not isfile(DB_NAME):
    raise Exception("\nСоздайте таблицы командой:\nsqlite3 chat.db < test3.sql")

db = sqlite3.connect(DB_NAME)
cursor = db.cursor()
cursor.executemany("insert into users (name) values (?)", USERS)
cursor.executemany("insert into messages (uid, msg) values (?, ?)", MESSAGES)
db.commit()

cursor.execute("""
   select u.name, count(m.id) from users u
   join messages m on m.uid = u.id
   group by u.id;
   """)

print "Количество сообщений от пользователей:"
for user, count in cursor:
    print user, count

cursor.execute("""
   select u.name, count(m.id) from users u
   left join messages m on m.uid = u.id
   group by u.id
   having count(m.id) = 0;
   """)

print "Пользователи не отправлявшие сообщений:"
for user, count in cursor:
    print user, count

db.close()
