BACKEND = backend/server.native

FRONTEND = frontend/index.html

all: build

$(BACKEND):
	$(MAKE) -C backend build

$(FRONTEND):
	$(MAKE) -C frontend build

build: $(BACKEND) $(FRONTEND)

run: build
	./$(BACKEND)

run-spa:
	./node_modules/.bin/single-page-server --base=frontend/build --file=frontend/build/index.html --port=8085

.PHONY: $(BACKEND) $(FRONTEND) build run
