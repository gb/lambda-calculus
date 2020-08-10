stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build

run:
	$(stack) run

install:
	$(stack) install

test:
	$(stack) test

.DEFAULT_GOAL := run
