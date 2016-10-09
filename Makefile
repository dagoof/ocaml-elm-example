BACKEND = backend/server.native

FRONTEND = frontend/index.html

all: build

setup:
	npm install https://github.com/daviesian/single-page-server
	opam pin add ocaml_elm_example backend -y
	$(MAKE) -C frontend $@

$(BACKEND):
	$(MAKE) -C backend build
	chmod +x $@

$(FRONTEND):
	$(MAKE) -C frontend build

build: $(BACKEND) $(FRONTEND)

test:
	$(MAKE) -C backend $@

run: build
	./$(BACKEND)

run-spa:
	./node_modules/.bin/single-page-server --base=frontend/build --file=frontend/build/index.html --port=8085

.PHONY: $(BACKEND) $(FRONTEND) build run test
