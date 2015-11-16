use wikipedia;
drop table if exists urls;
create table urls (id int AUTO_INCREMENT primary key, url nvarchar(1000)) COLLATE utf8_bin;
alter table urls add unique index url_index (`url`);
drop table if exists words;
create table words(id int AUTO_INCREMENT primary key, word nvarchar(1000)) collate utf32_bin;
alter table words add unique index word_index (`word`);