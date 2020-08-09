stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build

run:
	$(stack) run

install:
	$(stack) install

ghci:
	$(stack) ghci $(package):lib --ghci-options='-j6 +RTS -A128m'

test:
	$(stack) test

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests --ghci-options='-j6 +RTS -A128m'

bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind -j4 +RTS -A128m' --main-is $(package):$(package)"

dev-deps:
	stack install ghcid

.PHONY : build run install ghci test bench test-ghci ghcid dev-deps
