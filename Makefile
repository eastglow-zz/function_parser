src=src/function_parser.for src/main.for 
FC=gfortran
FFLAGS=-O2 -Wall
.PHONY: main clean

main: $(src)
	$(FC) $(FFLAGS) -o main.exe $(src)