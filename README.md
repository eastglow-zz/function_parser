# function_parser

This is a minimal Fortran library for parsing and evaluating mathematical functions given as strings. It supports basic arithmetic operations, parentheses, and common mathematical functions.
## Features
- Parse mathematical expressions from strings
- Evaluate expressions with variable substitution
- Support for common mathematical functions (sin, cos, exp, log, etc.)

## List of Supported Operators and Functions
### Operators
- Addition: `+`
- Subtraction: `-`
- Multiplication: `*`
- Division: `/`
- Exponentiation: `**`
- Modulus: `mod(a, b)`
### Functions
- Sine: `sin(x)`
- Cosine: `cos(x)`
- Tangent: `tan(x)`
- Exponential: `exp(x)`
- Natural Logarithm: `log(x)`
- Minimum: `min(a, b)`
- Maximum: `max(a, b)`
### Constants
- Pi: `pi`
- Euler's Number: `e`
- Gas Constant: `R`
## Installation
To compile the library, use the provided Makefile:
```
make main
```
This will generate an executable named `main.exe`.
## Usage
To use the library, include the `function_parser.for` file in your Fortran project and call the provided functions to parse and evaluate expressions.
## Example
Here is a simple example of how to use the library:
```fortran
program test_function_parser
  use function_parser
  implicit none 
  character(len=100) :: expr
  real :: result
  expr = "sin(x) + 2*x^2"
  call fp_tokenization(trim(expr))
  call fp_infix_to_postfix()
  write(*,*) "Evaluating the expression..."
  write(*,*) "Result: ", fp_evaluate_postfix()
end program test_function_parser  

```
## License
This project is licensed under the LGPL2.1 License. See the LICENSE file for details.
## Contributing
Contributions are welcome! Please open an issue or submit a pull request for any improvements or bug fixes.
## Acknowledgments
AI assistance provided by Visual Studio Code Copilot.
I didn't even know what infix and postfix were, and still don't fully understand them (actually I don't care that much). It took about a couple of hours to write this library with Copilot's help.

## Makefile
```makefile
src=src/main.for src/function_parser.for
FC=gfortran
FFLAGS=-O2 -Wall
.PHONY: main clean

main: $(src)
	$(FC) $(FFLAGS) -o main.exe $(src)

clean:
	rm -f main.exe  
``` 
