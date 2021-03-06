run: build
	stack exec postgresql-simple-example
	stack exec hasql-vanilla-example
	stack exec hasql-generic-example
	stack exec persistent-example
	stack exec esqueleto-example
	stack exec opaleye-example

build:
	stack build

init:
	docker run -d -p 5432:5432 postgres
	while true; do psql "host=localhost port=5432 user=postgres" -f init.sql && break || sleep 1; done

psql:
	psql "host=localhost port=5432 user=postgres"

shell:
	nix-shell --command 'export PS1="[haskell-sql] \t \# \h $$? $$ "; return'
