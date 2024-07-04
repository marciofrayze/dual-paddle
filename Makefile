.PHONY: help build-prod

help:
	@echo "\n\nAvailable commands:\n"
	@echo "run-elm-watch-hot: run locally in dev mode with hot reload using elm-watch.\n"
	@echo "build-prod: build the production version of the webapp using elm-watch.\n"
	@echo "\n"

run-elm-watch-hot:
	npx elm-watch hot

run-local-http-server:
	http-server

build-prod:
	npx elm-watch make --optimize
	@say -v "Trinoids" Finished building production version of the webapp
