
up:
	docker-compose up -d

down:
	docker-compose down	

build:
	cabal build

local_run: build
	PG_DSN=postgresql://postgres:example@localhost:25432/shorter cabal exec shorter	