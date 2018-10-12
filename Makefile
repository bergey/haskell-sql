run: build
	stack exec postgresql-simple-example
	stack exec hasql-vanilla-example
	stack exec hasql-generic-example
	stack exec persistent-example
	stack exec esqueleto-example

build:
	stack build

init:
	docker run -d -p 5432:5432 postgres
	psql "host=localhost port=5432 user=postgres" -f init.sql

psql:
	psql "host=localhost port=5432 user=postgres"
