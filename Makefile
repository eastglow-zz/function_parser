src=src/main.for src/function_parser.for
FC=gfortran
FFLAGS=-O2 -Wall
.PHONY: main clean

main: $(src)
	$(FC) $(FFLAGS) -o main.exe $(src)