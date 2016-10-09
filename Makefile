BACKEND = backend/server.native

FRONTEND = frontend/index.html

all: build

setup:
	npm install single-page-server
	opam install opium lwt yojson ppx_deriving_yojson batteries cohttp rresult alcotest
	$(MAKE) -C frontend $@

$(BACKEND):
	$(MAKE) -C backend build

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
