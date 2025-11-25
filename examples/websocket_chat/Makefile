.PHONY: run test clean matcha build

run: matcha
	gleam run -m main

test:
	gleam test

matcha:
	@matcha

build: matcha
	@gleam build

clean:
	gleam clean

