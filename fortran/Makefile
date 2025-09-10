COLS ?= 10
# ITRS ?= 5

run: build
	@./program $(COLS)

build: program.f90 Makefile
	@gfortran -o program program.f90
