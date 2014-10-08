create table if not exists users (
  id integer primary key autoincrement not null,
  name varchar(30)
);

create table if not exists messages (
  id integer primary key autoincrement not null,
  uid integer not null,
  msg varchar(30)
);
