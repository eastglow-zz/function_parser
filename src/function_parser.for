      module function_parser
      use, intrinsic :: iso_fortran_env, dp => real64
      implicit none

      integer(kind=int32),parameter :: fp_token_max_size = 30
      integer(kind=int32),parameter :: fp_max_stack_size = 100
      integer(kind=int32),parameter :: fp_max_user_defined_variables = 
     &                                                               500
      character(len=3),parameter :: fp_reserved_tokens(15) = 
     &  ['** ', 'exp', 'log', 'sin', 'cos', 'tan', 'mod', 'min', 'max',
     &   '+  ', '-  ', '*  ', '/  ', '(  ', ')  ']
      integer(kind=int32),parameter :: fp_num_operand(15) = 
     &  [ 2   ,  1   ,  1   ,  1   ,  1   ,  1   ,  2   ,  2   ,  2   ,
     &    2   ,  2   ,  2   ,  2   ,  0   ,  0  ]
      character(len=fp_token_max_size) :: 
     &                                 fp_token_stack(fp_max_stack_size)
      character(len=fp_token_max_size) :: 
     &                               fp_reserve_stack(fp_max_stack_size)
      integer(kind=int32) :: fp_token_stack_size = 0

      character(len=fp_token_max_size) :: fp_known_functions(9) = 
     &  ['exp', 'log', 'sin', 'cos', 'tan', 'mod', 'min', 'max', '** ']
      character(len=fp_token_max_size) :: fp_known_constants(2) = 
     &  ['pi', 'e ']
      real(kind=dp) :: fp_known_constants_values(2) = 
     &  [3.141592653589793_dp, 2.718281828459045_dp]
      character(len=fp_token_max_size) :: fp_known_variables(9) = 
     &  ['x    ', 'y    ', 'z    ', 't    ', 'r    ', 'theta', 'phi  ', 
     &   'T    ', 'P    ']
      real(kind=dp) :: fp_known_variables_values(9) = 
     &  [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp,
     &   0.0_dp]
      character(len=fp_token_max_size) :: 
     &          fp_user_defined_variables(fp_max_user_defined_variables)
      real(kind=dp) :: 
     &   fp_user_defined_variables_values(fp_max_user_defined_variables)
      integer(kind=int32) :: fp_num_user_defined_variables = 0

      contains 

      ! ----------------------------------------------------------------

      subroutine fp_tokenization(expression)
      implicit none 
      character(len=*), intent(in) :: expression

      ! Save the tokens into fp_token_stack and return the number of tokens
      integer(kind=int32) :: i, j, len_expression, token_len
      character(len=fp_token_max_size) :: current_token
      integer(kind=int32) :: num_tokens
      logical :: is_operator

      len_expression = len_trim(expression)
      num_tokens = 0
      i = 1
      
      do while (i <= len_expression)
        ! Skip spaces
        if (expression(i:i) == ' ') then
          i = i + 1
          cycle
        end if
        
        ! Check for reserved tokens (operators/functions)
        is_operator = .false.
        do j = 1, size(fp_reserved_tokens)
          token_len = len_trim(fp_reserved_tokens(j))
          if (i + token_len - 1 <= len_expression) then
            if (expression(i:i+token_len-1) == 
     &          trim(fp_reserved_tokens(j))) then
              num_tokens = num_tokens + 1
              fp_token_stack(num_tokens) = trim(fp_reserved_tokens(j))
              i = i + token_len
              is_operator = .true.
              exit
            end if
          end if
        end do
        
        ! If not an operator, accumulate characters for number/variable
        if (.not. is_operator) then
          current_token = ''
          do while (i <= len_expression)
            ! Check if current position is start of an operator
            is_operator = .false.
            do j = 1, size(fp_reserved_tokens)
              token_len = len_trim(fp_reserved_tokens(j))
              if (i + token_len - 1 <= len_expression) then
                if (expression(i:i+token_len-1) == 
     &              trim(fp_reserved_tokens(j))) then
                  is_operator = .true.
                  exit
                end if
              end if
            end do
            if (is_operator .or. expression(i:i) == ' ') exit
            
            current_token = trim(current_token) // expression(i:i)
            i = i + 1
          end do
          
          if (len_trim(current_token) > 0) then
            num_tokens = num_tokens + 1
            fp_token_stack(num_tokens) = trim(current_token)
          end if
        end if
      end do

      fp_token_stack_size = num_tokens

      return 
      end subroutine fp_tokenization

      ! ----------------------------------------------------------------

      subroutine fp_infix_to_postfix()
      implicit none

      ! Convert infix expression in fp_token_stack to postfix notation
      integer(kind=int32) :: i, j, top_reserve
      character(len=fp_token_max_size) :: token, top_token
      integer(kind=int32) :: precedence(fp_max_stack_size)
      character(len=fp_token_max_size) :: 
     &                                   output_stack(fp_max_stack_size)
      integer(kind=int32) :: output_size
      output_size = 0
      top_reserve = 0
      precedence = 0
      do i = 1, fp_token_stack_size
        token = fp_token_stack(i)
        ! Handle different token types here (operands, operators, parentheses)
        ! This is a placeholder for the actual implementation
        ! You would implement the Shunting Yard algorithm or similar here
        ! Shunting Yard algorithm implementation goes here
        ! Check if token is a number or variable (operand)
        if (.not. any(fp_reserved_tokens == token)) then
          output_size = output_size + 1
          output_stack(output_size) = token
        
        ! Handle left parenthesis
        else if (trim(token) == '(') then
          top_reserve = top_reserve + 1
          fp_reserve_stack(top_reserve) = token
        
        ! Handle right parenthesis
        else if (trim(token) == ')') then
          do while (top_reserve > 0)
            if (trim(fp_reserve_stack(top_reserve)) == '(') then
              top_reserve = top_reserve - 1
              exit
            end if
            output_size = output_size + 1
            output_stack(output_size) = fp_reserve_stack(top_reserve)
            top_reserve = top_reserve - 1
          end do
        
        ! Handle operators and functions
        else
          ! Assign precedence levels
          if (trim(token) == '**') then
            precedence(i) = 4
          else if (trim(token) == 'exp' .or. trim(token) == 'log' .or.
     &             trim(token) == 'sin' .or. trim(token) == 'cos' .or.
     &             trim(token) == 'tan' .or. trim(token) == 'mod' .or.
     &             trim(token) == 'min' .or. trim(token) == 'max') then
            precedence(i) = 5
          else if (trim(token) == '*' .or. trim(token) == '/') then
            precedence(i) = 3
          else if (trim(token) == '+' .or. trim(token) == '-') then
            precedence(i) = 2
          end if
          
          ! Pop operators with higher or equal precedence
          do while (top_reserve > 0)
            top_token = fp_reserve_stack(top_reserve)
            if (trim(top_token) == '(') exit
            
            ! Find precedence of top token
            do j = 1, i-1
              if (trim(fp_token_stack(j)) == trim(top_token)) then
                if (precedence(j) >= precedence(i)) then
                  output_size = output_size + 1
                  output_stack(output_size) = top_token
                  top_reserve = top_reserve - 1
                  exit
                else
                  exit
                end if
              end if
            end do
            if (trim(fp_reserve_stack(top_reserve)) == trim(top_token))
     &        exit
          end do
          
          ! Push current operator
          top_reserve = top_reserve + 1
          fp_reserve_stack(top_reserve) = token
        end if
      end do
            
      ! Pop remaining operators
      do while (top_reserve > 0)
        output_size = output_size + 1
        output_stack(output_size) = fp_reserve_stack(top_reserve)
        top_reserve = top_reserve - 1
      end do
            
      ! Copy output back to token stack
      do i = 1, output_size
        fp_token_stack(i) = output_stack(i)
      end do
      fp_token_stack_size = output_size


      return 
      end subroutine fp_infix_to_postfix

      ! ----------------------------------------------------------------
      
      real(kind=dp) function fp_evaluate_postfix()
      implicit none
      ! Evaluate the postfix expression in fp_token_stack
      integer(kind=int32) :: i, j, top_operand
      character(len=fp_token_max_size) :: token
      real(kind=dp) :: operand_stack(fp_max_stack_size)
      real(kind=dp) :: op1, op2, result
      integer(kind=int32) :: ios
      top_operand = 0
      do i = 1, fp_token_stack_size
        token = fp_token_stack(i)
        ! Check if token is a number
        read(token, *, iostat=ios) result
        if (ios == 0) then
          top_operand = top_operand + 1
          operand_stack(top_operand) = result
        else
          ! Handle operators and functions
          select case (trim(token))
          case ('+')
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1 + op2
          case ('-')
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1 - op2
          case ('*')
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1 * op2
          case ('/')
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1 / op2
          ! Add more operators and functions as needed
          case( '**')
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1 ** op2
          case ('sin')
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = sin(op1)
          case ('cos')
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = cos(op1)
          case ('exp')
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = exp(op1)
          case ('log')
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = log(op1)
          case ('mod')
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = mod(op1, op2)
          case ('min')
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = min(op1, op2)
          case ('max')
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = max(op1, op2)
          case('ln')
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = log(op1)
          end select

          !if the token is known constant
          do j = 1, size(fp_known_constants)
            if (trim(token) == trim(fp_known_constants(j))) then
              top_operand = top_operand + 1
              operand_stack(top_operand) = fp_known_constants_values(j)
            end if
          end do

          !if the token is known variable
          do j = 1, size(fp_known_variables)
            if (trim(token) == trim(fp_known_variables(j))) then
              top_operand = top_operand + 1
              operand_stack(top_operand) = fp_known_variables_values(j)
            end if
          end do

          !if the token is user-defined variable
          do j = 1, fp_num_user_defined_variables
            if (trim(token) == 
     &          trim(fp_user_defined_variables(j))) then
              top_operand = top_operand + 1
              operand_stack(top_operand) = 
     &          fp_user_defined_variables_values(j)
            end if
          end do

        end if !if (j == 0) then ... else ...
      end do ! do i = 1, fp_token_stack_size

      fp_evaluate_postfix = operand_stack(top_operand)

      return 
      end function fp_evaluate_postfix

      ! ----------------------------------------------------------------

      subroutine fp_print_tokens()
      implicit none
      integer(kind=int32) :: i
      write(*,*) "Tokens:"
      do i = 1, fp_token_stack_size
        if (trim(fp_token_stack(i)) /= '') then
          write(*,*) "  ", trim(fp_token_stack(i))
        end if
      end do

      return 
      end subroutine fp_print_tokens

      ! ----------------------------------------------------------------



      end module function_parser