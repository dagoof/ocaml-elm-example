build: build/index.html

build/index.html: src/Main.elm src/*.elm
	-@mkdir -p $(@D)
	elm-make --output $@ $<

setup:
	elm-package install -y

.PHONY: setup
