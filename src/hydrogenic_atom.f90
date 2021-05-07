!>
program hydrogenic_atom

  use laguerre

  implicit none

  ! debugging flag
  logical , parameter :: debugging = .true.

  ! angular quantum number variables
  integer :: l, m
  real :: alpha

  ! atomic variables
  ! z - atomic charge
  integer :: z

  ! radial grid variables
  integer :: n_r
  real :: d_r, r_max
  real , allocatable :: r_grid(:)

  ! basis variables
  integer :: n_basis
  real , allocatable :: basis(:, :)
  real , allocatable :: eigen_basis(:, :)

  ! matrices
  real , allocatable :: B(:, :), K(:, :), V(:, :), H(:, :)
  real , allocatable :: eigen_values(:), eigen_vectors(:, :)

  ! local variables
  integer :: ii, jj, kk
  integer :: ierr = 0

  ! read parameters from command line arguments
  call read_input(l, m, alpha, z, n_basis, d_r, r_max)
  n_r = ceiling(r_max / d_r)

  ! allocate arrays
  allocate(r_grid(n_r))

  allocate(basis(n_r, n_basis))
  allocate(eigen_basis(n_r, n_basis))

  allocate(B(n_basis, n_basis))
  allocate(K(n_basis, n_basis))
  allocate(V(n_basis, n_basis))
  allocate(H(n_basis, n_basis))

  allocate(eigen_values(n_basis))
  allocate(eigen_vectors(n_basis, n_basis))

  ! initialise radial grid
  do ii = 1, n_r
    r_grid(ii) = d_r * ii
  end do

  ! calculate radial basis functions
  call radial_basis(l, alpha, n_r, r_grid, n_basis, basis)

  if (debugging) then
    write (*, *) "basis(n_r, n_basis)"
    call display_radial_basis(n_r, n_basis, basis)
  end if

  ! calculate matrix elements
  call hydrogenic_matrices(l, m, alpha, z, n_basis, B, K, V, H)

  if (debugging) then
    write (*, *) "B(n_basis, n_basis)"
    call display_matrix(n_basis, n_basis, B)

    write (*, *) "K(n_basis, n_basis)"
    call display_matrix(n_basis, n_basis, K)

    write (*, *) "V(n_basis, n_basis)"
    call display_matrix(n_basis, n_basis, V)

    write (*, *) "H(n_basis, n_basis)"
    call display_matrix(n_basis, n_basis, H)
  end if

  ! solve matrix equations
  eigen_values(:) = 0.0
  eigen_vectors(:, :) = 0.0

  call rsg(n_basis, n_basis, H, B, eigen_values, 1, eigen_vectors, ierr)

  if (ierr /= 0) then
    write (*, "(a, i4)") "rsg failed with error code: ", ierr
    call exit(ierr)
  end if

  if (debugging) then
    write (*, *) "eigen_values(n_basis)"
    call display_vector(n_basis, eigen_values)

    write (*, *) "eigen_vectors(n_basis, n_basis)"
    call display_matrix(n_basis, n_basis, eigen_vectors)
  end if

  ! calculate eigen-states
  eigen_basis(:, :) = 0.0
  do jj = 1, n_basis
    do ii = 1, n_r
      do kk = 1, n_basis
        eigen_basis(ii, jj) = eigen_basis(ii, jj) &
            + (eigen_vectors(kk, jj) * basis(ii, kk))
      end do
    end do
  end do

  if (debugging) then
    write (*, *) "eigen_basis(n_r, n_basis)"
    call display_radial_basis(n_r, n_basis, eigen_basis)
  end if

  ! write output to file

  ! deallocate arrays
  deallocate(r_grid)

  deallocate(basis)
  deallocate(eigen_basis)

  deallocate(B)
  deallocate(K)
  deallocate(V)
  deallocate(H)

  deallocate(eigen_values)
  deallocate(eigen_vectors)

contains

  ! read_input
  subroutine read_input (l, m, alpha, z, n_basis, d_r, r_max)
    integer , intent(out) :: l, m, z, n_basis
    real , intent(out) :: alpha, d_r, r_max
    integer :: num_args
    character(len=20) :: arg

    num_args = command_argument_count()

    if (num_args >= 1) then
      call get_command_argument(1, arg)
      read (arg, *) l
    else
      write (*, *) "<l> not specified, using default value of 0"
      l = 0
    end if

    if (num_args >= 2) then
      call get_command_argument(2, arg)
      read (arg, *) m
    else
      write (*, *) "<m> not specified, using default value of 0"
      m = 0
    end if

    if (num_args >= 3) then
      call get_command_argument(3, arg)
      read (arg, *) alpha
    else
      write (*, *) "<alpha> not specified, using default value of 1.0"
      alpha = 1.0
    end if

    if (num_args >= 4) then
      call get_command_argument(4, arg)
      read (arg, *) z
    else
      write (*, *) "<z> not specified, using default value of 1"
      z = 1
    end if

    if (num_args >= 5) then
      call get_command_argument(5, arg)
      read (arg, *) n_basis
    else
      write (*, *) "<n_basis> not specified, using default value of 10"
      n_basis = 1
    end if

    if (num_args >= 6) then
      call get_command_argument(6, arg)
      read (arg, *) d_r
    else
      write (*, *) "<d_r> not specified, using default value of 0.1"
      d_r = 0.1
    end if

    if (num_args >= 7) then
      call get_command_argument(7, arg)
      read (arg, *) r_max
    else
      write (*, *) "<r_max> not specified, using default value of 100.0"
      r_max = 10.0
    end if

  end subroutine read_input

  ! write_output
  ! subroutine write_output ()
  ! end subroutine write_output

  ! display_vector
  subroutine display_vector (n, x)
    integer , intent(in) :: n
    real , intent(in) :: x(n)
    integer :: ii
    integer :: w, d
    character(len=50) :: fmt, str_w, str_d, str_zero

    ! determine real number formatting
    d = 4
    w = max(ceiling(log10(maxval(abs(basis(:, :))))), 1) + d + 3

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
  subroutine display_matrix (n_rows, n_cols, A)
    integer , intent(in) :: n_rows, n_cols
    real , intent(in) :: A(n_rows, n_cols)
    integer :: ii, jj
    integer :: w, d
    character(len=50) :: fmt, str_w, str_d, str_zero

    ! determine real number formatting
    d = 4
    w = max(ceiling(log10(maxval(abs(basis(:, :))))), 1) + d + 3

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

  ! display_radial_basis
  subroutine display_radial_basis (n_r, n_basis, basis)
    integer , intent(in) :: n_r, n_basis
    real , intent(in) :: basis(n_r, n_basis)
    integer :: ii, jj
    integer :: w, d
    character(len=50) :: fmt, str_w, str_d, str_zero

    ! determine real number formatting
    d = 4
    w = max(ceiling(log10(maxval(abs(basis(:, :))))), 1) + d + 3

    write (str_w, *) w
    write (str_d, *) d

    write (fmt, *) "(f", trim(adjustl(str_w)), ".", trim(adjustl(str_d)), ")"

    str_zero = repeat(' ', w)

    ! write out radial basis values
    do ii = 1, n_r
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

  end subroutine display_radial_basis

end program hydrogenic_atom
