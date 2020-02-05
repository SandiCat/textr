create table fruit (
    id serial primary key,
    name varchar not null,
    sugar_content float
);

create table post (
    id serial primary key,
    body varchar not null,
    nickname varchar not null,
    age integer not null,
    gender varchar not null
);