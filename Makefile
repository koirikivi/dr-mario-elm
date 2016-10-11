install:
	npm install elm elm-live
	node node_modules/elm/binwrappers/elm-package install --yes

develop:
	node node_modules/elm-live/bin/elm-live.js Game.elm

.PHONY: install develop
