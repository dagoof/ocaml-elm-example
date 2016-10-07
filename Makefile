build:
	$(MAKE) -C backend build
	$(MAKE) -C frontend build

.PHONY: build
