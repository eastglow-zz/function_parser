
      program main 
      use, intrinsic :: iso_fortran_env, dp => real64

      use function_parser
      
      implicit none

      character(len=1000) :: math_expression
      
      write(*,*)"Enter a mathematical expression (e.g., 3.5 + 2.1 * 4):"
      !read(*,'(A)') math_expression
      !write(*,*) "You entered the expression: ", trim(math_expression)
      !write(*,*) "Set variable values from the code: "
      !write(*,*) "x = 5.0" 
      !call fp_set_variable_value("x", 5.0_dp)
      call fp_get_string_from_file("./math_exp.txt", math_expression)
      write(*,*) "Reading the expression from the file 'math_exp.txt'.."
      write(*,*) "Expression: ", trim(math_expression)
      write(*,*) "Evaluating the expression..."
      write(*,*) "Result: ", fp_evaluate(trim(math_expression))
      

      end program main 