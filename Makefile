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
	mkdir -p build/build
	mv build/main.js build/build/main.js
	mkdir -p build/js
	cp -r js/* build/js
	cp index.html build/index.html
	@say -v "Trinoids" Finished building dual paddle for web.
	open build/index.html 
