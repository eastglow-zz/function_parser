      module function_parser
      use iso_fortran_env 
      implicit none

      integer(kind=int32),parameter :: fp_token_max_size = 30
      integer(kind=int32),parameter :: fp_max_stack_size = 100
            character(len=3),parameter :: fp_reserved_tokens(15) = 
     &  ['** ', 'exp', 'log', 'sin', 'cos', 'tan', 'mod', 'min', 'max',
     &   '+  ', '-  ', '*  ', '/  ', '(  ', ')  ']
      character(len=fp_token_max_size) :: 
     &                                 fp_token_stack(fp_max_stack_size)
      character(len=fp_token_max_size) :: 
     &                               fp_reserve_stack(fp_max_stack_size)
      integer(kind=int32) :: fp_token_stack_size = -1


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