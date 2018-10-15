all:
	@echo "make what?"

elm-dev:
	cd gui-src/elm && elm-live --dir ../../src/static/ --pushstate -- --output ../../src/static/elm.js ./src/Main.elm

elm-prod:
	./scripts/build-elm-prod.sh

backup-assets-tarball:
	cd src && tar cjf static-assets.tar.bz2 static && mv static-assets.tar.bz2 ../../simsapa-db-backup/

