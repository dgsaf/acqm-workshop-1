!>
program hydrogenic_atom

  use laguerre
  use io

  implicit none

  ! global flags
  logical , parameter :: debugging = .true.
  logical , parameter :: display_bases = .false.
  logical , parameter :: display_matrices = .true.

  ! angular quantum number variables
  integer :: l, m
  double precision :: alpha

  ! atomic variables
  integer :: atomic_charge

  ! radial grid variables
  integer :: n_r
  double precision :: d_r, r_max
  double precision , allocatable :: r_grid(:)

  ! basis variables
  integer :: n_basis
  double precision , allocatable :: basis(:, :)
  double precision , allocatable :: eigen_basis(:, :)

  ! matrices
  double precision , allocatable :: B(:, :), K(:, :), V(:, :), H(:, :)
  double precision , allocatable :: eigen_values(:), eigen_vectors(:, :)

  ! local variables
  integer :: ii

  ! read parameters from command line arguments
  call read_input(l, m, alpha, atomic_charge, n_basis, d_r, r_max)
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

  if (display_bases) then
    write (*, *) "basis(n_r, n_basis)"
    call display_basis(n_r, r_grid, n_basis, basis)
  end if

  ! calculate matrix elements
  call hydrogenic_matrices(l, m, alpha, atomic_charge, n_basis, B, K, V, H)

  if (display_matrices) then
    write (*, *) "B(n_basis, n_basis)"
    call display_matrix(n_basis, n_basis, B)

    write (*, *) "K(n_basis, n_basis)"
    call display_matrix(n_basis, n_basis, K)

    write (*, *) "V(n_basis, n_basis)"
    call display_matrix(n_basis, n_basis, V)

    write (*, *) "H(n_basis, n_basis)"
    call display_matrix(n_basis, n_basis, H)
  end if

  ! diagonalise
  call diagonalise(n_r, n_basis, basis, B, H, eigen_values, eigen_vectors, &
      eigen_basis)

  if (display_matrices) then
    write (*, *) "eigen_values(n_basis)"
    call display_vector(n_basis, eigen_values)

    write (*, *) "eigen_vectors(n_basis, n_basis)"
    call display_matrix(n_basis, n_basis, eigen_vectors)
  end if

  if (display_bases) then
    write (*, *) "eigen_basis(n_r, n_basis)"
    call display_basis(n_r, r_grid, n_basis, eigen_basis)
  end if

  ! write output to file
  call write_output(l, m, alpha, atomic_charge, n_r, r_grid, n_basis, basis, &
      B, K, V, H, eigen_values, eigen_vectors, eigen_basis)

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

  ! diagonalise
  ! Note that since the call to rsg modifies the matrices it is given, we send
  ! it copies of B, H.
  subroutine diagonalise (n_r, n_basis, basis, B, H, eigen_values, &
      eigen_vectors, eigen_basis)
    integer , intent(in) :: n_r, n_basis
    double precision , intent(in) :: basis(n_r, n_basis)
    double precision , intent(in) :: B(n_basis, n_basis), H(n_basis, n_basis)
    double precision , intent(out) :: eigen_values(n_basis)
    double precision , intent(out) :: eigen_vectors(n_basis, n_basis)
    double precision , intent(out) :: eigen_basis(n_r, n_basis)
    double precision :: B_copy(n_basis, n_basis), H_copy(n_basis, n_basis)
    integer :: ii, jj, kk
    integer :: ierr
    double precision :: temp_sum

    ! create copies of B, H matrices to use in call to rsg subroutine
    B_copy(:, :) = B(:, :)
    H_copy(:, :) = H(:, :)

    ! solve eigenvalue matrix equation
    eigen_values(:) = 0.0
    eigen_vectors(:, :) = 0.0

    call rsg(n_basis, n_basis, H_copy, B_copy, eigen_values, 1, eigen_vectors, &
        ierr)

    if (ierr /= 0) then
      write (*, "(a, i5)") "rsg failed with error code: ", ierr
      call exit(ierr)
    end if

    ! calculate eigen-basis
    eigen_basis(:, :) = 0.0

    do jj = 1, n_basis
      do ii = 1, n_r
        temp_sum = 0.0

        do kk = 1, n_basis
          temp_sum = temp_sum + (eigen_vectors(kk, jj) * basis(ii, kk))
        end do

        eigen_basis(ii, jj) = temp_sum
      end do
    end do

  end subroutine diagonalise

end program hydrogenic_atom
