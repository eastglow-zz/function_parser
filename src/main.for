
      program main 
      use iso_fortran_env

      use function_parser
      
      implicit none

      character(len=1000) :: math_expression
      
      write(*,*)"Enter a mathematical expression (e.g., 3.5 + 2.1 * 4):"
      read(*,'(A)') math_expression
      write(*,*) "You entered the expression: ", trim(math_expression)

      call fp_tokenization(trim(math_expression))
      call fp_print_tokens()

      end program main 