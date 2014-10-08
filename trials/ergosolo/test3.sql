create table if not exists users (
  uid integer primary key autoincrement not null,
  name varchar(30)
);

create table if not exists messages (
  uid integer primary key autoincrement not null,
  id integer not null,
  msg varchar(30)
);
