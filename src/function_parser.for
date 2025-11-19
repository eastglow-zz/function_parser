      module function_parser
      use, intrinsic :: iso_fortran_env, dp => real64
      implicit none

      integer(kind=int32),parameter :: fp_token_max_size = 30
      integer(kind=int32),parameter :: fp_expression_str_size = 200
      integer(kind=int32),parameter :: fp_var_def_str_size = 100
      integer(kind=int32),parameter :: fp_max_stack_size = 100
      integer(kind=int32),parameter :: fp_max_user_defined_variables = 
     &                                                               500
      character(len=11),parameter :: fp_reserved_tokens(41) = 
     &  ['**         ',
     &   'exp        ',
     &   'log10      ',
     &   'log_gamma  ',
     &   'ln         ',
     &   'log        ',
     &   'erfc_scaled',
     &   'erfc       ',
     &   'erf        ',
     &   'bessel_j0  ',
     &   'bessel_j1  ',
     &   'bessel_jn  ',
     &   'bessel_y0  ',
     &   'bessel_y1  ',
     &   'bessel_yn  ',
     &   'acosh      ',
     &   'acos       ',
     &   'asinh      ',
     &   'asin       ',
     &   'atan2      ',
     &   'atanh      ',
     &   'atan       ',
     &   'cosh       ',
     &   'sinh       ',
     &   'tanh       ',
     &   'sin        ',
     &   'cos        ',
     &   'tan        ',
     &   'gamma      ',
     &   'hypot      ',
     &   'mod        ',
     &   'min        ',
     &   'max        ',
     &   'abs        ',
     &   'sqrt       ',
     &   '+          ',
     &   '-          ',
     &   '*          ',
     &   'div        ',
     &   '(          ', 
     &   ')          ']
        
      character(len=fp_token_max_size) :: 
     &                                 fp_token_stack(fp_max_stack_size)
      character(len=fp_token_max_size) :: 
     &                               fp_reserve_stack(fp_max_stack_size)
      integer(kind=int32) :: fp_token_stack_size = 0

      character(len=fp_token_max_size) :: fp_known_functions(35) = 
     &  ['**         ',
     &   'exp        ',
     &   'ln         ',
     &   'log        ',
     &   'sin        ',
     &   'cos        ',
     &   'tan        ',
     &   'mod        ',
     &   'min        ',
     &   'max        ',
     &   'abs        ',
     &   'sqrt       ',
     &   'acos       ',
     &   'acosh      ',
     &   'asin       ',
     &   'asinh      ',
     &   'atan       ',
     &   'atan2      ',
     &   'atanh      ',
     &   'cosh       ',
     &   'sinh       ',
     &   'tanh       ',
     &   'log10      ',
     &   'hypot      ',
     &   'bessel_j0  ',
     &   'bessel_j1  ',
     &   'bessel_jn  ',
     &   'bessel_y0  ',
     &   'bessel_y1  ',
     &   'bessel_yn  ',
     &   'erf        ',
     &   'erfc       ',
     &   'erfc_scaled',
     &   'gamma      ',
     &   'log_gamma  ']
      character(len=fp_token_max_size) :: fp_known_constants(3) = 
     &  ['pi', 'e ', 'R ']
      real(kind=dp) :: fp_known_constants_values(3) = 
     &  [3.141592653589793_dp, 2.718281828459045_dp, 8.314462618_dp]
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
      character(len=fp_expression_str_size) :: fp_expression_string
      character(len=fp_var_def_str_size),allocatable :: 
     &                                             fp_var_defs_string(:)
      integer(kind=int32) :: fp_num_var_defs = 0

      contains 

      ! ----------------------------------------------------------------

      subroutine fp_get_string_from_file(file_path, output_string)
      implicit none
      character(len=*), intent(in) :: file_path
      character(len=*), intent(out) :: output_string
      integer :: unit_num, ios, file_len
      logical :: exists
      character(len=1000) :: line

      ! The input string saved in the file expected in multiple lines. We read all lines and concatenate them.
      ! Sanitation check
      inquire(file=file_path, exist=exists)
      if (.not. exists) then
        write(*,*) "Error: File not found - ", trim(file_path)
        output_string = ''
        return
      end if

      ! Identify the file length
      open(newunit=unit_num, file=trim(file_path), status='old', 
     &                                        action='read', iostat=ios)
      if (ios /= 0) then
        write(*,*) "Error: Unable to open file - ", trim(file_path)
        output_string = ''
        return
      end if
      file_len = 0
      do
        read(unit_num, '(A)', iostat=ios) output_string
        if (ios /= 0) exit
        file_len = file_len + len_trim(output_string)
      end do
      close(unit_num)

      ! Read the file content again to get the full string
      open(newunit=unit_num, file=trim(file_path), status='old',
     &                                        action='read', iostat=ios)
      if (ios /= 0) then
        write(*,*) "Error: Unable to open file - ", trim(file_path)
        output_string = ''
        return
      end if
      output_string = ''
      do
        
        read(unit_num, '(A)', iostat=ios) line
        if (ios /= 0) exit
        output_string = trim(output_string) // trim(line)
      end do
      close(unit_num)

      ! Verbose of the read string
      write(*,*) "Verbose from fp_get_string_from_file:"
      write(*,*) "  Successfully read from file: ", trim(file_path)
      write(*,*) "  Read expression from file: ", trim(output_string)
      
      return
      end subroutine fp_get_string_from_file

      ! ----------------------------------------------------------------

      function fp_to_lowercase(str) result(lower_str)
      implicit none
      character(len=*), intent(in) :: str
      character(len=len(str)) :: lower_str
      integer :: i, ic
      
      lower_str = str
      do i = 1, len(str)
        ic = ichar(str(i:i))
        if (ic >= ichar('A') .and. ic <= ichar('Z')) then
          lower_str(i:i) = char(ic + ichar('a') - ichar('A'))
        end if
      end do
      end function fp_to_lowercase

      ! ----------------------------------------------------------------

      subroutine fp_tokenization(expression)
      implicit none
      character(len=*), intent(in) :: expression

      ! Save the tokens into fp_token_stack and return the number of tokens
      integer(kind=int32) :: i, j, len_expression, token_len
      character(len=fp_token_max_size) :: current_token
      integer(kind=int32) :: num_tokens
      logical :: is_operator, has_dot, has_exp, exp_sign_allowed
      character(len=1) :: c

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
            ! Case-insensitive comparison for alphabetic tokens
            if ((fp_reserved_tokens(j)(1:1) >= 'a' .and.
     &           fp_reserved_tokens(j)(1:1) <= 'z') .or.
     &          (fp_reserved_tokens(j)(1:1) >= 'A' .and.
     &           fp_reserved_tokens(j)(1:1) <= 'Z')) then
              ! Alphabetic - case-insensitive
              if (fp_to_lowercase(expression(i:i+token_len-1)) == 
     &            trim(fp_reserved_tokens(j))) then
                num_tokens = num_tokens + 1
                fp_token_stack(num_tokens) = trim(fp_reserved_tokens(j))
                i = i + token_len
                is_operator = .true.
                exit
              end if
            else
              ! Non-alphabetic - direct comparison
              if (expression(i:i+token_len-1) == 
     &            trim(fp_reserved_tokens(j))) then
                num_tokens = num_tokens + 1
                fp_token_stack(num_tokens) = trim(fp_reserved_tokens(j))
                i = i + token_len
                is_operator = .true.
                exit
              end if
            end if
          end if
        end do
        
        ! Special handling for / -> div
        if (.not. is_operator .and. expression(i:i) == '/') then
          num_tokens = num_tokens + 1
          fp_token_stack(num_tokens) = 'div'
          i = i + 1
          is_operator = .true.
        end if

        ! If not an operator, accumulate characters for number/variable
        if (.not. is_operator) then
          current_token = ''
          c = expression(i:i)
          ! Number (allow scientific notation like 1E-9)
          if ((c >= '0' .and. c <= '9') .or. c == '.') then
            has_dot = .false.
            has_exp = .false.
            exp_sign_allowed = .false.
            do while (i <= len_expression)
              c = expression(i:i)
              if (c >= '0' .and. c <= '9') then
                current_token = trim(current_token) // c
                exp_sign_allowed = .false.
              else if (c == '.' .and. .not. has_dot .and. 
     &                 .not. has_exp) then
                has_dot = .true.
                current_token = trim(current_token) // c
                exp_sign_allowed = .false.
              else if ((c == 'e' .or. c == 'E') .and. 
     &                 .not. has_exp) then
                has_exp = .true.
                current_token = trim(current_token) // c
                exp_sign_allowed = .true.
              else if ((c == '+' .or. c == '-') .and. 
     &                 exp_sign_allowed) then
                ! sign after E/e
                current_token = trim(current_token) // c
                exp_sign_allowed = .false.
              else
                exit
              end if
              i = i + 1
            end do
          else
            ! parse variable/identifier until operator or space
            do while (i <= len_expression)
              ! Check if current position is start of an operator
              is_operator = .false.
              do j = 1, size(fp_reserved_tokens)
                token_len = len_trim(fp_reserved_tokens(j))
                if (i + token_len - 1 <= len_expression) then
                  ! Case-insensitive comparison for alphabetic tokens
                  if ((fp_reserved_tokens(j)(1:1) >= 'a' .and.
     &                 fp_reserved_tokens(j)(1:1) <= 'z') .or.
     &                (fp_reserved_tokens(j)(1:1) >= 'A' .and.
     &                 fp_reserved_tokens(j)(1:1) <= 'Z')) then
                    ! Alphabetic - case-insensitive
                    if (fp_to_lowercase(expression(i:i+token_len-1)) == 
     &                  trim(fp_reserved_tokens(j))) then
                      is_operator = .true.
                      exit
                    end if
                  else
                    ! Non-alphabetic - direct comparison
                    if (expression(i:i+token_len-1) == 
     &                  trim(fp_reserved_tokens(j))) then
                      is_operator = .true.
                      exit
                    end if
                  end if
                end if
              end do
              if (is_operator .or. expression(i:i) == ' ' .or. 
     &            expression(i:i) == '/') exit

              current_token = trim(current_token) // expression(i:i)
              i = i + 1
            end do
          end if

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

      subroutine fp_extract_expression_and_variable_definitions(
     &                                                    input_string)
      implicit none
      character(len=*), intent(in) :: input_string

      character(len=fp_expression_str_size) :: expression
      character(len=fp_var_def_str_size) :: var_defs_string
      integer(kind=int32) :: num_var_defs


      ! Variable definitions are expected in the format: [var1=val1, var2=val2, ...]
      integer(kind=int32) :: i, len_input, start_var_defs, end_var_defs
      integer(kind=int32) :: var_start, var_end, var_index

      len_input = len_trim(input_string)
      start_var_defs = index(input_string, '[')
      end_var_defs = index(input_string, ']')
      if (start_var_defs > 0 .and. end_var_defs > start_var_defs) then
        var_defs_string = 
     &               trim(input_string(start_var_defs+1:end_var_defs-1))
        ! Check if expression is before or after the brackets
        if (start_var_defs == 1) then
          ! Format: [var1=val1, var2=val2] expression
          expression = trim(input_string(end_var_defs+1:len_input))
        else
          ! Format: expression [var1=val1, var2=val2]
          expression = trim(input_string(1:start_var_defs-1))
        end if
        fp_expression_string = trim(expression)
      else
        expression = trim(input_string)
        var_defs_string = ''
        fp_expression_string = trim(expression)
      end if
      ! Count number of variable definitions
      num_var_defs = 0
      do i = 1, len_trim(var_defs_string)
        if (var_defs_string(i:i) == ',') then
          num_var_defs = num_var_defs + 1
        end if
      end do
      if (len_trim(var_defs_string) > 0) then
        num_var_defs = num_var_defs + 1
      end if

      fp_num_var_defs = num_var_defs
      allocate(fp_var_defs_string(fp_num_var_defs))
      ! Save variable definitions into fp_var_defs_string
      if (num_var_defs > 0) then
        var_start = 1
        var_index = 1
        do i = 1, len_trim(var_defs_string)
          if (var_defs_string(i:i) == ',' .or. 
     &                              i == len_trim(var_defs_string)) then
            if (i == len_trim(var_defs_string)) then
              var_end = i
            else
              var_end = i - 1
            end if
            fp_var_defs_string(var_index) = 
     &                 trim(adjustl(var_defs_string(var_start:var_end)))
            var_index = var_index + 1
            var_start = i + 1
          end if
        end do
      end if

      return 
      end subroutine fp_extract_expression_and_variable_definitions

      ! ----------------------------------------------------------------

      subroutine fp_set_variable_value(var_name, var_value)
      implicit none
      character(len=*), intent(in) :: var_name
      real(kind=dp), intent(in) :: var_value
      integer(kind=int32) :: i
      logical :: found
      found = .false.
      ! Check if var_name is a known variable
      do i = 1, size(fp_known_variables)
        if (trim(var_name) == trim(fp_known_variables(i))) then
          fp_known_variables_values(i) = var_value
          found = .true.
          exit
        end if
      end do
      ! If not known variable, check if it's already a user-defined variable
      if (.not. found) then
        do i = 1, fp_num_user_defined_variables
          if (trim(var_name) == 
     &        trim(fp_user_defined_variables(i))) then
            fp_user_defined_variables_values(i) = var_value
            found = .true.
            exit
          end if
        end do
      end if
      ! If still not found, add as new user-defined variable
      if (.not. found) then
        fp_num_user_defined_variables = 
     &                                 fp_num_user_defined_variables + 1
        fp_user_defined_variables(fp_num_user_defined_variables) = 
     &                                                    trim(var_name)
        fp_user_defined_variables_values(
     &                        fp_num_user_defined_variables) = var_value
      end if

      return 
      end subroutine fp_set_variable_value

      ! ----------------------------------------------------------------

      subroutine fp_update_var_values_by_var_defs()
      implicit none
      ! Update the variable values based on fp_var_defs_string. If it's a known variable stored in fp_known_variables,
      ! update its value in fp_known_variables_values. If it's a new variable, add it to fp_user_defined_variables, and update its value in fp_user_defined_variables_values.
      integer(kind=int32) :: i, j, eq_pos, len_var_def
      character(len=fp_token_max_size) :: var_name, var_value_str
      real(kind=dp) :: var_value
      integer(kind=int32) :: ios
      logical :: found
      do i = 1, fp_num_var_defs
        len_var_def = len_trim(fp_var_defs_string(i))
        eq_pos = index(fp_var_defs_string(i), '=')
        if (eq_pos > 0) then
          var_name = trim(adjustl(fp_var_defs_string(i)(1:eq_pos-1)))
          var_value_str = 
     &        trim(adjustl(fp_var_defs_string(i)(eq_pos+1:len_var_def)))
          read(var_value_str, *, iostat=ios) var_value
          if (ios == 0) then
            found = .false.
            ! Check if var_name is a known variable
            do j = 1, size(fp_known_variables)
              if (trim(var_name) == trim(fp_known_variables(j))) then
                fp_known_variables_values(j) = var_value
                found = .true.
                exit
              end if
            end do
            ! If not known variable, check if it's already a user-defined variable
            if (.not. found) then
              do j = 1, fp_num_user_defined_variables
                if (trim(var_name) == 
     &              trim(fp_user_defined_variables(j))) then
                  fp_user_defined_variables_values(j) = var_value
                  found = .true.
                  exit
                end if
              end do
            end if
            ! If still not found, add as new user-defined variable
            if (.not. found) then
              fp_num_user_defined_variables = 
     &                                 fp_num_user_defined_variables + 1
              fp_user_defined_variables(fp_num_user_defined_variables) =
     &                                                    trim(var_name)
              fp_user_defined_variables_values(
     &                        fp_num_user_defined_variables) = var_value
            end if
          end if
        end if
      end do

      ! Removed debug output - uncomment if needed for debugging
      !if (fp_num_var_defs > 0) then
      !  write(*,*) "Updated variable values from definitions."
      !  write(*,*) "Number of variables defined: ", fp_num_var_defs
      !  do i = 1, fp_num_var_defs
      !    write(*,*) "  ", trim(fp_var_defs_string(i))
      !  end do
      !end if

      return 
      end subroutine fp_update_var_values_by_var_defs

      ! ----------------------------------------------------------------

      subroutine fp_infix_to_postfix()
      implicit none

      ! Convert infix expression in fp_token_stack to postfix notation
      integer(kind=int32) :: i, j, top_reserve
      character(len=fp_token_max_size) :: token, top_token
      character(len=fp_token_max_size) :: 
     &                                   output_stack(fp_max_stack_size)
      integer(kind=int32) :: output_size
      integer(kind=int32) :: prec_token, prec_top
      logical :: is_right_assoc
      logical :: prev_is_reserved, is_reserved

      output_size = 0
      top_reserve = 0

      do i = 1, fp_token_stack_size
        token = fp_token_stack(i)

        ! Detect unary + / -: if at start or previous token is operator or '('
        ! But ')' should NOT make next +/- unary (it's end of expression)
        if (trim(token) == '+' .or. trim(token) == '-') then
          if (i == 1) then
            token = 'u' // trim(token)
          else
            prev_is_reserved = .false.
            if (trim(fp_token_stack(i-1)) /= ')') then
              do j = 1, size(fp_reserved_tokens)
                if (trim(fp_reserved_tokens(j)) == 
     &              trim(fp_token_stack(i-1))) then
                  prev_is_reserved = .true.
                  exit
                end if
              end do
            end if
            if (trim(fp_token_stack(i-1)) == '(' .or. 
     &          prev_is_reserved) then
              token = 'u' // trim(token)
            end if
          end if
        end if

        ! If token is an operand (number or variable)
        is_reserved = .false.
        if (trim(token) /= 'u+' .and. trim(token) /= 'u-') then
          do j = 1, size(fp_reserved_tokens)
            if (trim(fp_reserved_tokens(j)) == trim(token)) then
              is_reserved = .true.
              exit
            end if
          end do
        else
          is_reserved = .true.
        end if
        if (.not. is_reserved) then
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
          ! Determine precedence for current token
          is_right_assoc = .false.
          if (trim(token) == '**') then
            prec_token = 6
            is_right_assoc = .true.
          else if (trim(token) == 'u-' .or. trim(token) == 'u+') then
            prec_token = 5
          else if (trim(token) == 'exp' .or. trim(token) == 'ln' .or.
     &         trim(token) == 'log' .or.
     &         trim(token) == 'sin' .or. trim(token) == 'cos' .or.
     &         trim(token) == 'tan' .or. trim(token) == 'mod' .or.
     &         trim(token) == 'abs' .or. trim(token) == 'sqrt' .or.
     &         trim(token) == 'min' .or. trim(token) == 'max' .or.
     &         trim(token) == 'acos' .or. trim(token) == 'acosh' .or.
     &         trim(token) == 'asin' .or. trim(token) == 'asinh' .or.
     &         trim(token) == 'atan' .or. trim(token) == 'atan2' .or.
     &         trim(token) == 'atanh' .or. trim(token) == 'cosh' .or.
     &         trim(token) == 'sinh' .or. trim(token) == 'tanh' .or.
     &         trim(token) == 'log10' .or. trim(token) == 'hypot' .or.
     &         trim(token) == 'bessel_j0' .or. 
     &         trim(token) == 'bessel_j1' .or.
     &         trim(token) == 'bessel_jn' .or. 
     &         trim(token) == 'bessel_y0' .or.
     &         trim(token) == 'bessel_y1' .or. 
     &         trim(token) == 'bessel_yn' .or.
     &         trim(token) == 'erf' .or. trim(token) == 'erfc' .or.
     &         trim(token) == 'erfc_scaled' .or.
     &         trim(token) == 'gamma' .or. 
     &         trim(token) == 'log_gamma') then
            prec_token = 7
          else if (trim(token) == '*' .or. trim(token) == 'div') then
            prec_token = 4
          else if (trim(token) == '+' .or. trim(token) == '-') then
            prec_token = 3
          else
            prec_token = 1
          end if

          ! Pop operators from reserve stack according to precedence
          do while (top_reserve > 0)
            top_token = trim(fp_reserve_stack(top_reserve))
            if (top_token == '(') exit

            ! determine precedence of top token
            if (top_token == '**') then
              prec_top = 6
            else if (top_token == 'u-' .or. top_token == 'u+') then
              prec_top = 5
            else if (top_token == 'exp' .or. top_token == 'ln' .or.
     &           top_token == 'log' .or.
     &           top_token == 'sin' .or. top_token == 'cos' .or.
     &           top_token == 'tan' .or. top_token == 'mod' .or.
     &           top_token == 'abs' .or. top_token == 'sqrt' .or.
     &           top_token == 'min' .or. top_token == 'max' .or.
     &           top_token == 'acos' .or. top_token == 'acosh' .or.
     &           top_token == 'asin' .or. top_token == 'asinh' .or.
     &           top_token == 'atan' .or. top_token == 'atan2' .or.
     &           top_token == 'atanh' .or. top_token == 'cosh' .or.
     &           top_token == 'sinh' .or. top_token == 'tanh' .or.
     &           top_token == 'log10' .or. top_token == 'hypot' .or.
     &           top_token == 'bessel_j0' .or. 
     &           top_token == 'bessel_j1' .or.
     &           top_token == 'bessel_jn' .or. 
     &           top_token == 'bessel_y0' .or.
     &           top_token == 'bessel_y1' .or. 
     &           top_token == 'bessel_yn' .or.
     &           top_token == 'erf' .or. top_token == 'erfc' .or.
     &           top_token == 'erfc_scaled' .or.
     &           top_token == 'gamma' .or. 
     &           top_token == 'log_gamma') then
              prec_top = 7
            else if (top_token == '*' .or. top_token == 'div') then
              prec_top = 4
            else if (top_token == '+' .or. top_token == '-') then
              prec_top = 3
            else
              prec_top = 1
            end if

            if (is_right_assoc) then
              if (prec_top > prec_token) then
                output_size = output_size + 1
                output_stack(output_size) = 
     &              fp_reserve_stack(top_reserve)
                top_reserve = top_reserve - 1
              else
                exit
              end if
            else
              if (prec_top >= prec_token) then
                output_size = output_size + 1
                output_stack(output_size) = 
     &              fp_reserve_stack(top_reserve)
                top_reserve = top_reserve - 1
              else
                exit
              end if
            end if
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
      real(kind=dp) :: op1, op2, temp_val
      integer(kind=int32) :: ios
      top_operand = 0
      do i = 1, fp_token_stack_size
        token = fp_token_stack(i)
        ! Check if token is a number
        read(token, *, iostat=ios) temp_val
        if (ios == 0) then
          top_operand = top_operand + 1
          operand_stack(top_operand) = temp_val
        else
          ! Handle operators and functions
          ! Use if-then-else instead of select case to avoid issues with '/'
          if (trim(token) == '+') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1 + op2
          else if (trim(token) == '-') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1 - op2
          else if (trim(token) == '*') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1 * op2
          else if (trim(token) == 'div') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1 / op2
          else if (trim(token) == '**') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1 ** op2
          else if (trim(token) == 'u-') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = -op1
          else if (trim(token) == 'u+') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = op1
          else if (trim(token) == 'sin') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = sin(op1)
          else if (trim(token) == 'cos') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = cos(op1)
          else if (trim(token) == 'exp') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = exp(op1)
          else if (trim(token) == 'log' .or. trim(token) == 'ln') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = log(op1)
          else if (trim(token) == 'mod') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = mod(op1, op2)
          else if (trim(token) == 'min') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = min(op1, op2)
          else if (trim(token) == 'max') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = max(op1, op2)
          else if (trim(token) == 'abs') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = abs(op1)
          else if (trim(token) == 'sqrt') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = sqrt(op1)
          else if (trim(token) == 'tan') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = tan(op1)
          else if (trim(token) == 'acos') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = acos(op1)
          else if (trim(token) == 'acosh') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = acosh(op1)
          else if (trim(token) == 'asin') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = asin(op1)
          else if (trim(token) == 'asinh') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = asinh(op1)
          else if (trim(token) == 'atan') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = atan(op1)
          else if (trim(token) == 'atan2') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = atan2(op1, op2)
          else if (trim(token) == 'atanh') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = atanh(op1)
          else if (trim(token) == 'cosh') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = cosh(op1)
          else if (trim(token) == 'sinh') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = sinh(op1)
          else if (trim(token) == 'tanh') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = tanh(op1)
          else if (trim(token) == 'log10') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = log10(op1)
          else if (trim(token) == 'hypot') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = hypot(op1, op2)
          else if (trim(token) == 'bessel_j0') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = bessel_j0(op1)
          else if (trim(token) == 'bessel_j1') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = bessel_j1(op1)
          else if (trim(token) == 'bessel_jn') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = bessel_jn(int(op1), op2)
          else if (trim(token) == 'bessel_y0') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = bessel_y0(op1)
          else if (trim(token) == 'bessel_y1') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = bessel_y1(op1)
          else if (trim(token) == 'bessel_yn') then
            op2 = operand_stack(top_operand)
            top_operand = top_operand - 1
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = bessel_yn(int(op1), op2)
          else if (trim(token) == 'erf') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = erf(op1)
          else if (trim(token) == 'erfc') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = erfc(op1)
          else if (trim(token) == 'erfc_scaled') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = erfc_scaled(op1)
          else if (trim(token) == 'gamma') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = gamma(op1)
          else if (trim(token) == 'log_gamma') then
            op1 = operand_stack(top_operand)
            operand_stack(top_operand) = log_gamma(op1)
          else
            ! Token is not an operator, check if it's a constant or variable
            !if the token is known constant
            do j = 1, size(fp_known_constants)
              if (trim(token) == trim(fp_known_constants(j))) then
                top_operand = top_operand + 1
                operand_stack(top_operand) = 
     &              fp_known_constants_values(j)
                exit
              end if
            end do

            !if the token is known variable
            do j = 1, size(fp_known_variables)
              if (trim(token) == trim(fp_known_variables(j))) then
                top_operand = top_operand + 1
                operand_stack(top_operand) = 
     &              fp_known_variables_values(j)
                exit
              end if
            end do

            !if the token is user-defined variable
            do j = 1, fp_num_user_defined_variables
              if (trim(token) == 
     &            trim(fp_user_defined_variables(j))) then
                top_operand = top_operand + 1
                operand_stack(top_operand) = 
     &            fp_user_defined_variables_values(j)
                exit
              end if
            end do
          end if

        end if !if (ios == 0) then ... else ...
      end do ! do i = 1, fp_token_stack_size

      fp_evaluate_postfix = operand_stack(top_operand)

      return 
      end function fp_evaluate_postfix

      ! ----------------------------------------------------------------

      subroutine fp_print_tokens()
      implicit none
      integer(kind=int32) :: i
      open(unit=99, file='tokens_debug.txt', status='replace')
      write(99,*) "Tokens:"
      do i = 1, fp_token_stack_size
        if (trim(fp_token_stack(i)) /= '') then
          write(99,*) "  ", trim(fp_token_stack(i))
        end if
      end do
      close(99)

      return 
      end subroutine fp_print_tokens

      ! ----------------------------------------------------------------

      real(kind=dp) function fp_evaluate(str)
      implicit none
      character(len=*), intent(in) :: str

      call fp_extract_expression_and_variable_definitions(str) 
      call fp_update_var_values_by_var_defs()
      call fp_tokenization(fp_expression_string)
      call fp_infix_to_postfix()
      !call fp_print_tokens()  ! Uncomment for debugging
      fp_evaluate = fp_evaluate_postfix()

      !if fp_var_defs_string is allocated, deallocate it
      if (allocated(fp_var_defs_string)) then
        deallocate(fp_var_defs_string)
      end if

      end function fp_evaluate

      ! ----------------------------------------------------------------

      end module function_parser