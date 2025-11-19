# function_parser

This is a minimal Fortran library for parsing and evaluating mathematical functions given as strings. It supports basic arithmetic operations, parentheses, common mathematical functions, scientific notation, unary operators, and variable definitions.

## Features
- Parse mathematical expressions from strings
- Evaluate expressions with variable substitution
- Support for 35+ mathematical functions (trigonometric, hyperbolic, logarithmic, special functions)
- **Case-insensitive function names** (e.g., `SIN`, `sin`, `Sin` all work)
- **Case-sensitive variable names** (e.g., `x` and `X` are different variables)
- Scientific notation support (e.g., 1E-9, 2.5E3)
- Unary operators (+/-)
- User-defined variables with inline definitions
- Built-in constants (pi, e, R)

## Installation
To compile the library, use the provided Makefile:
```
make main
```
This will generate an executable named `main.exe`.

## Usage

### Basic Usage
The library provides a simple interface through the `fp_evaluate` function:

```fortran
program example
  use function_parser
  implicit none 
  character(len=100) :: expr
  real(kind=real64) :: result
  
  expr = "3.5 + 2.1 * 4"
  result = fp_evaluate(trim(expr))
  write(*,*) "Result: ", result
end program example
```

### Reading Expressions from a File
You can read mathematical expressions from a text file using `fp_get_string_from_file`:

```fortran
program example
  use function_parser
  implicit none 
  character(len=1000) :: expr
  real(kind=real64) :: result
  
  ! Read expression from file
  call fp_get_string_from_file("./math_exp.txt", expr)
  
  ! Evaluate the expression
  result = fp_evaluate(trim(expr))
  write(*,*) "Result: ", result
end program example
```

**Notes:**
- The file can contain multiple lines; they will be concatenated into a single expression
- The subroutine provides verbose output showing the file path and expression read
- If the file is not found or cannot be opened, an error message is displayed and an empty string is returned

### Setting Variables from Code
You can set variable values programmatically using `fp_set_variable_value` before evaluating an expression:

```fortran
program example
  use function_parser
  implicit none 
  character(len=100) :: expr
  real(kind=real64) :: result
  
  ! Set variable values from code
  call fp_set_variable_value("x", 5.0_dp)
  call fp_set_variable_value("y", 3.0_dp)
  call fp_set_variable_value("myvar", 10.5_dp)
  
  ! Evaluate expression using the set variables
  expr = "x**2 + y**2 + myvar"
  result = fp_evaluate(trim(expr))
  write(*,*) "Result: ", result  ! 25 + 9 + 10.5 = 44.5
end program example
```

**Notes:**
- Works with both built-in variables (`x`, `y`, `z`, `t`, `r`, `theta`, `phi`, `T`, `P`) and user-defined variables
- If the variable doesn't exist, it will be created automatically
- Variable names are case-sensitive
- This method is useful when variable values are computed or come from other parts of your program

### Variable Definitions (Inline)
You can also define variables inline within the expression using square brackets. Both formats are supported:

```
[var1=value1, var2=value2] expression
```
or
```
expression [var1=value1, var2=value2]
```

**Examples:**
```
[myvar1=23, myvar2=22] myvar1*myvar2
myvar1*myvar2 [myvar1=23, myvar2=22]
x + y [x=3, y=2]
```

### Scientific Notation
Numbers can be expressed in scientific notation:
```
1E-9        (0.000000001)
2.5E3       (2500.0)
-1.5E-2     (-0.015)
```

### Unary Operators
Unary plus and minus are supported:
```
-5 + 3      (-2)
3 * -2      (-6)
(-5 + 3) * 2  (-4)
5 - -3      (8)
```

### Complex Expressions
```
exp(-200000/R/1000)
sin(pi/4) + cos(pi/4)
sqrt(x**2 + y**2) [x=3, y=4]
SINH(1) + COSH(1)         # Case-insensitive functions
gamma(5)                  # Gamma(5) = 4! = 24
log10(1000) + LN(e)       # Mixed case works: 3 + 1 = 4
[r=-10] r + R             # Case-sensitive variables: -10 + 8.314
```

## List of Supported Operators and Functions
### Operators
- Addition: `+`
- Subtraction: `-`
- Multiplication: `*`
- Division: `/`
- Exponentiation: `**`
- Unary Plus: `+x`
- Unary Minus: `-x`

### Functions
**Note:** All function names are case-insensitive (e.g., `SIN`, `sin`, `Sin` all work the same).

#### Trigonometric Functions
- Sine: `sin(x)`
- Cosine: `cos(x)`
- Tangent: `tan(x)`
- Arcsine: `asin(x)`
- Arccosine: `acos(x)`
- Arctangent: `atan(x)`
- Two-argument arctangent: `atan2(y, x)`

#### Hyperbolic Functions
- Hyperbolic sine: `sinh(x)`
- Hyperbolic cosine: `cosh(x)`
- Hyperbolic tangent: `tanh(x)`
- Inverse hyperbolic sine: `asinh(x)`
- Inverse hyperbolic cosine: `acosh(x)`
- Inverse hyperbolic tangent: `atanh(x)`

#### Logarithmic and Exponential Functions
- Exponential: `exp(x)`
- Natural logarithm: `log(x)` or `ln(x)`
- Base-10 logarithm: `log10(x)`
- Logarithm of gamma function: `log_gamma(x)`

#### Special Functions
- Gamma function: `gamma(x)`
- Error function: `erf(x)`
- Complementary error function: `erfc(x)`
- Scaled complementary error function: `erfc_scaled(x)`
- Bessel function J0: `bessel_j0(x)`
- Bessel function J1: `bessel_j1(x)`
- Bessel function Jn: `bessel_jn(n, x)` - n must be integer
- Bessel function Y0: `bessel_y0(x)`
- Bessel function Y1: `bessel_y1(x)`
- Bessel function Yn: `bessel_yn(n, x)` - n must be integer
- Euclidean distance: `hypot(x, y)` - returns sqrt(x²+y²)

#### Other Mathematical Functions
- Modulus: `mod(a, b)`
- Minimum: `min(a, b)`
- Maximum: `max(a, b)`
- Absolute value: `abs(x)`
- Square root: `sqrt(x)`

### Constants
**Note:** Constant names are case-sensitive.
- Pi: `pi` (3.141592653589793)
- Euler's Number: `e` (2.718281828459045)
- Gas Constant: `R` (8.314462618)

### Built-in Variables
**Note:** Variable names are case-sensitive (e.g., `x` and `X` are different variables).

The following variables are available and can be set using variable definitions:
- `x`, `y`, `z`, `t`, `r`, `theta`, `phi`, `T`, `P`

## Example
Here is a simple example of how to use the library:
```fortran
program test_function_parser
  use function_parser
  implicit none 
  character(len=200) :: expr
  real(kind=real64) :: result
  
  ! Simple arithmetic
  expr = "3.5 + 2.1 * 4"
  result = fp_evaluate(trim(expr))
  write(*,*) "Result: ", result  ! 11.9
  
  ! With variables (case-sensitive)
  expr = "[x=5, y=3] x**2 + y**2"
  result = fp_evaluate(trim(expr))
  write(*,*) "Result: ", result  ! 34.0
  
  ! Case-insensitive functions
  expr = "SIN(pi/2) + COS(0)"
  result = fp_evaluate(trim(expr))
  write(*,*) "Result: ", result  ! 2.0
  
  ! Scientific notation and special functions
  expr = "gamma(5) * exp(-1E-3)"
  result = fp_evaluate(trim(expr))
  write(*,*) "Result: ", result  ! ~23.976
  
  ! Hyperbolic functions
  expr = "SINH(1) + COSH(1)"
  result = fp_evaluate(trim(expr))
  write(*,*) "Result: ", result  ! e ≈ 2.718
end program test_function_parser  
```

## Implementation Notes

### Case Sensitivity
- **Function names are case-insensitive**: `SIN(x)`, `sin(x)`, and `Sin(x)` all work identically
- **Variable names are case-sensitive**: `x` and `X` are treated as different variables
- **Constant names are case-sensitive**: Use lowercase `pi`, `e`, `r` for the built-in constants

### Division Operator
Due to Fortran fixed-form syntax limitations with the `/` character, the division operator is internally represented as `div` token. This is handled automatically during tokenization and is transparent to the user.

### Operator Precedence
The library implements standard mathematical operator precedence:
1. Functions (sin, cos, exp, etc.) - Highest precedence
2. Exponentiation (`**`) - Right associative
3. Unary operators (`+`, `-`)
4. Multiplication and Division (`*`, `/`)
5. Addition and Subtraction (`+`, `-`) - Lowest precedence
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
