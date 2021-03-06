!>
module io

  implicit none

  ! global flags
  integer , parameter :: dp_default = 4

contains

  ! read_input
  subroutine read_input (l, m, alpha, atomic_charge, n_basis, d_r, r_max)
    integer , intent(out) :: l, m, atomic_charge, n_basis
    double precision , intent(out) :: alpha, d_r, r_max
    integer :: num_args
    character(len=20) :: arg

    num_args = command_argument_count()

    if (num_args < 7) then
      write (*, *) "arguments are: ",  &
          "<l> <m> <alpha> <atomic_charge> <n_basis> <d_r> <r_max>"
    end if

    if (num_args >= 1) then
      call get_command_argument(1, arg)
      read (arg, *) l
    else
      write (*, *) "> <l> not specified, using default value of 0"
      l = 0
    end if

    if (num_args >= 2) then
      call get_command_argument(2, arg)
      read (arg, *) m
    else
      write (*, *) "> <m> not specified, using default value of 0"
      m = 0
    end if

    if (num_args >= 3) then
      call get_command_argument(3, arg)
      read (arg, *) alpha
    else
      write (*, *) "> <alpha> not specified, using default value of 1.0"
      alpha = 1.0d0
    end if

    if (num_args >= 4) then
      call get_command_argument(4, arg)
      read (arg, *) atomic_charge
    else
      write (*, *) "> <atomic_charge> not specified, using default value of 1"
      atomic_charge = 1
    end if

    if (num_args >= 5) then
      call get_command_argument(5, arg)
      read (arg, *) n_basis
    else
      write (*, *) "> <n_basis> not specified, using default value of 10"
      n_basis = 10
    end if

    if (num_args >= 6) then
      call get_command_argument(6, arg)
      read (arg, *) d_r
    else
      write (*, *) "> <d_r> not specified, using default value of 0.1"
      d_r = 0.1d0
    end if

    if (num_args >= 7) then
      call get_command_argument(7, arg)
      read (arg, *) r_max
    else
      write (*, *) "> <r_max> not specified, using default value of 100.0"
      r_max = 100.0d0
    end if

    write (*, *) "parameters: "
    write (*, *) "> <l>:             ", l
    write (*, *) "> <m>:             ", m
    write (*, *) "> <alpha>:         ", alpha
    write (*, *) "> <atomic_charge>: ", atomic_charge
    write (*, *) "> <n_basis>:       ", n_basis
    write (*, *) "> <d_r>:           ", d_r
    write (*, *) "> <r_max>:         ", r_max

  end subroutine read_input

  ! write_output
  subroutine write_output (l, m, alpha, atomic_charge, n_r, r_grid, n_basis, &
      basis, B, K, V, H, eigen_values, eigen_vectors, eigen_basis)
    integer , intent(in) :: l, m, atomic_charge, n_r, n_basis
    double precision , intent(in) :: alpha
    double precision , intent(in) :: r_grid(n_r)
    double precision , intent(in) :: basis(n_r, n_basis), &
        eigen_basis(n_r, n_basis)
    double precision , intent(in) :: B(n_basis, n_basis), K(n_basis, n_basis), &
        V(n_basis, n_basis), H(n_basis, n_basis), &
        eigen_vectors(n_basis, n_basis)
    double precision , intent(in) :: eigen_values(n_basis)
    character(len=1000) :: output_dir
    character(len=50) :: str_l, str_m, str_alpha, str_atomic_charge, str_n_basis

    ! construct output directory for given parameters
    write (str_l, *) l
    write (str_m, *) m
    write (str_alpha, "(f10.4)") alpha
    write (str_atomic_charge, *) atomic_charge
    write (str_n_basis, *) n_basis

    write (output_dir, *) &
        "output/", &
        "l-", trim(adjustl(str_l)), ".", &
        "m-", trim(adjustl(str_m)), ".", &
        "alpha-", trim(adjustl(str_alpha)), ".", &
        "atomic_charge-", trim(adjustl(str_atomic_charge)) , ".", &
        "n_basis-", trim(adjustl(str_n_basis)), "/"

    call execute_command_line("mkdir -p "//trim(adjustl(output_dir)))

    ! write basis functions to file
    call write_basis(n_r, r_grid, n_basis, basis, &
        trim(adjustl(output_dir))//"basis.txt")

    ! write matrices to file
    call write_matrix(n_basis, n_basis, B, &
        trim(adjustl(output_dir))//"B.txt")

    call write_matrix(n_basis, n_basis, K, &
        trim(adjustl(output_dir))//"K.txt")

    call write_matrix(n_basis, n_basis, V, &
        trim(adjustl(output_dir))//"V.txt")

    call write_matrix(n_basis, n_basis, H, &
        trim(adjustl(output_dir))//"H.txt")

    ! write eigen_values, eigen_vectors, eigen_basis functions to file
    call write_vector(n_basis, eigen_values, &
        trim(adjustl(output_dir))//"eigen_values.txt")

    call write_matrix(n_basis, n_basis, eigen_vectors, &
        trim(adjustl(output_dir))//"eigen_vectors.txt")

    call write_basis(n_r, r_grid, n_basis, eigen_basis, &
        trim(adjustl(output_dir))//"eigen_basis.txt")

  end subroutine write_output

  ! write_vector
  subroutine write_vector (n, x, vector_filename)
    integer , intent(in) :: n
    double precision , intent(in) :: x(n)
    character(len=*) , intent(in) :: vector_filename
    integer :: vector_file_unit
    integer :: ii

    ! open file
    vector_file_unit = 10

    open (unit=vector_file_unit, file=trim(adjustl(vector_filename)), &
        action="write")

    ! write matrix to file
    do ii = 1, n
      write (vector_file_unit, *) x(ii)
    end do

    ! close file
    close (vector_file_unit)

  end subroutine write_vector

  ! write_matrix
  subroutine write_matrix (n_rows, n_cols, A, matrix_filename)
    integer , intent(in) :: n_rows, n_cols
    double precision , intent(in) :: A(n_rows, n_cols)
    character(len=*) , intent(in) :: matrix_filename
    integer :: matrix_file_unit
    integer :: ii, jj

    ! open file
    matrix_file_unit = 10

    open (unit=matrix_file_unit, file=trim(adjustl(matrix_filename)), &
        action="write")

    ! write matrix to file
    do ii = 1, n_rows
      write (matrix_file_unit, *) A(ii, :)
    end do

    ! close file
    close (matrix_file_unit)

  end subroutine write_matrix

  ! write_basis
  subroutine write_basis (n_r, r_grid, n_basis, basis, basis_filename)
    integer , intent(in) :: n_r, n_basis
    double precision , intent(in) :: r_grid(n_r)
    double precision , intent(in) :: basis(n_r, n_basis)
    character(len=*) , intent(in) :: basis_filename
    integer :: basis_file_unit
    integer :: ii

    ! open file
    basis_file_unit = 10

    open (unit=basis_file_unit, file=trim(adjustl(basis_filename)), &
        action="write")

    ! write r_grid, and basis functions to file
    do ii = 1, n_r
      write (basis_file_unit, *) r_grid(ii), " ", basis(ii, :)
    end do

    ! close file
    close (basis_file_unit)

  end subroutine write_basis

  ! display_vector
  subroutine display_vector (n, x, dp)
    integer , intent(in) :: n
    double precision , intent(in) :: x(n)
    integer , optional , intent(in) :: dp
    integer :: w, d
    character(len=50) :: fmt, str_w, str_d, str_zero
    integer :: ii

    ! determine double precision number formatting
    if (present(dp)) then
      d = dp
    else
      d = dp_default
    end if

    w = max(ceiling(log10(maxval(abs(x(:))))), 1) + d + 3

    write (str_w, *) w
    write (str_d, *) d

    write (fmt, *) "(f", trim(adjustl(str_w)), ".", trim(adjustl(str_d)), ")"

    str_zero = repeat(' ', w)

    ! write out matrix elements
    do ii = 1, n
      ! if x(ii) will be written as "0.00..0", replace with " .     "
      if (abs(x(ii)) > (10.0**(-d))) then
        write (*, fmt) x(ii)
      else
        write (*, "(a, a, a)") &
            str_zero(1:w-d-1), ".", str_zero(w-d+1:w)
      end if

    end do

  end subroutine display_vector

  ! display_matrix
  subroutine display_matrix (n_rows, n_cols, A, dp)
    integer , intent(in) :: n_rows, n_cols
    double precision , intent(in) :: A(n_rows, n_cols)
    integer , optional, intent(in) :: dp
    integer :: w, d
    character(len=50) :: fmt, str_w, str_d, str_zero
    integer :: ii, jj

    ! determine double precision number formatting
    if (present(dp)) then
      d = dp
    else
      d = dp_default
    end if

    w = max(ceiling(log10(maxval(abs(A(:, :))))), 1) + d + 3

    write (str_w, *) w
    write (str_d, *) d

    write (fmt, *) "(f", trim(adjustl(str_w)), ".", trim(adjustl(str_d)), ")"

    str_zero = repeat(' ', w)

    ! write out matrix elements
    do ii = 1, n_rows
      do jj = 1, n_cols
        ! if A(ii, jj) will be written as "0.00..0", replace with " .     "
        if (abs(A(ii, jj)) > (10.0**(-d))) then
          write (*, fmt, advance="no") A(ii, jj)
        else
          write (*, "(a, a, a)", advance="no") &
              str_zero(1:w-d-1), ".", str_zero(w-d+1:w)
        end if
      end do
      write (*, *)
    end do

  end subroutine display_matrix

  ! display_basis
  subroutine display_basis (n_r, r_grid, n_basis, basis, dp)
    integer , intent(in) :: n_r, n_basis
    double precision , intent(in) :: r_grid(n_r)
    double precision , intent(in) :: basis(n_r, n_basis)
    integer , optional , intent(in) :: dp
    integer :: w, d
    character(len=50) :: fmt, str_w, str_d, str_zero
    integer :: ii, jj

    ! determine double precision number formatting
    if (present(dp)) then
      d = dp
    else
      d = dp_default
    end if

    w = max(ceiling(log10(maxval(abs(basis(:, :))))), 1) + d + 3

    write (str_w, *) w
    write (str_d, *) d

    write (fmt, *) "(f", trim(adjustl(str_w)), ".", trim(adjustl(str_d)), ")"

    str_zero = repeat(' ', w)

    ! write out radial basis values
    do ii = 1, n_r
      write (*, fmt, advance="no") r_grid(ii)
      do jj = 1, n_basis
        ! if basis(ii, jj) will be written as "0.00..0", replace with " .     "
        if (abs(basis(ii, jj)) > (10.0**(-d))) then
          write (*, fmt, advance="no") basis(ii, jj)
        else
          write (*, "(a, a, a)", advance="no") &
              str_zero(1:w-d-1), ".", str_zero(w-d+1:w)
        end if
      end do
      write (*, *)
    end do

  end subroutine display_basis

end module io
