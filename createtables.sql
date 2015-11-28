CREATE TABLE domains (
    domain varchar(128) CONSTRAINT pk PRIMARY KEY,
    email varchar(128) NOT NULL
);

INSERT INTO domains (domain, email) VALUES ('technion@lolware.net', 'lolware.net');

