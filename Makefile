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

.PHONY: $(BACKEND) $(FRONTEND) build run
