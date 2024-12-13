create table users (
       name text,
       password_hash text
);

create table images (
       id text,
       category text,
       pos integer,
       file blob
);

create table home (
       header text,
       presentation text
);
