build: build/index.html

build/index.html: src/Main.elm
	-@mkdir -p $(@D)
	elm-make --output $@ $<
