DROP TABLE IF EXISTS municipio;
DROP TABLE IF EXISTS estado;
DROP TABLE IF EXISTS regiao;

CREATE TABLE regiao (
  sigla TEXT		  PRIMARY KEY,
  nome 	 TEXT NOT NULL UNIQUE
);


INSERT INTO regiao (sigla, nome) VALUES ('N','Norte');
INSERT INTO regiao (sigla, nome) VALUES ('NE','Nordeste');
INSERT INTO regiao (sigla, nome) VALUES ('SE','Sudeste');
INSERT INTO regiao (sigla, nome) VALUES ('S','Sul');
INSERT INTO regiao (sigla, nome) VALUES ('CO','Centro-Oeste');