create table fruit (
    id serial primary key,
    name text not null,
    sugar_content float
);

create table post (
    id serial primary key,
    body text not null,
    nickname text not null,
    age integer not null,
    gender text not null
);