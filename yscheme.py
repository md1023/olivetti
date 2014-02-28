#!/usr/bin/python
from sqlalchemy.schema import Column, Table
from uuid import UUID

# class Users(object):
#     self.uid = 


# define_mapper(
#     Users, 



users_table = Table(
        "users",
        Column("uid", UUID, primary_key=True),
        Column("name", Unicode(255)))

messages_table = Table(
        "messages",
        Column("uid", UUID, ForeignKey("users.uid"), primary_key=True),
        Column("message", UnicodeText))

