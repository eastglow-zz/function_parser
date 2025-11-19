
      program main 
      use iso_fortran_env

      use function_parser
      
      implicit none

      character(len=1000) :: math_expression
      
      write(*,*)"Enter a mathematical expression (e.g., 3.5 + 2.1 * 4):"
      read(*,'(A)') math_expression
      write(*,*) "You entered the expression: ", trim(math_expression)
      write(*,*) "Evaluating the expression..."
      write(*,*) "Result: ", fp_evaluate(trim(math_expression))
      

      end program main 