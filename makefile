
up:
	docker-compose up -d

down:
	docker-compose down	

build:
	cabal build

server_local_run: build
	cabal exec shorter server

cli_local_run: build
	cabal exec shorter cli

upDb_local_run: build
	cabal exec shorter upDb	