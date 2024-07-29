.PHONY: help build-prod review review-fix

help:
	@echo "\n\nAvailable commands:\n"
	@echo "run-elm-watch-hot-and-http-server: run locally in dev mode with hot reload using elm-watch and http-server in parallel.\n"
	@echo "run-elm-watch-hot: run locally in dev mode with hot reload using elm-watch.\n"
	@echo "build-prod: build the production version of the webapp using elm-watch.\n"
	@echo "upgrade: upgrade the elm dependencies using elm-json.\n"
	@echo "upgrade-unsafe: upgrade the elm dependencies using elm-json with the --unsafe flag.\n"
	@echo "review: run elm-review.\n"
	@echo "review-fix: run elm-review with the --fix flag.\n"
	@echo "\n"

run-elm-watch-hot:
	npx elm-watch hot

run-elm-watch-hot-and-http-server:
	npx concurrently "npx elm-watch hot" "sleep 1 ; http-server" "sleep 1 ; open http://localhost:8080"

build-prod:
	npx elm-watch make --optimize
	mkdir -p build/build
	mv build/main.js build/build/main.js
	mkdir -p build/js
	cp -r js/* build/js
	cp index.html build/index.html
	@say -v "Trinoids" Finished building dual paddle for web.
	open build/index.html

upgrade:
	elm-json upgrade

upgrade-unsafe:
	elm-json upgrade --unsafe

review:
	elm-review

review-fix:
	elm-review --fix
