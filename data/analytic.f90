!>
program analytic

  implicit none

  ! basis variables
  integer :: l_max
  integer :: n_basis

  ! radial grid variables
  integer :: n_r
  double precision :: d_r, r_max
  double precision , allocatable :: r_grid(:)

  ! analytic basis radial functions
  double precision , allocatable :: analytic_basis(:, :)

  ! local variables
  integer :: ii, l, n
  character(len=50) :: filename

  ! set basis values
  l_max = 1
  n_basis = 3

  ! set grid values
  d_r = 0.1
  r_max = 100.0
  n_r = ceiling(r_max / d_r) + 1

  ! allocate arrays
  allocate(r_grid(n_r))
  allocate(analytic_basis(n_r, n_basis))

  ! initialise radial grid
  do ii = 1, n_r
    r_grid(ii) = d_r * (ii - 1)
  end do

  !
  do l = 0, l_max
    analytic_basis(:, :) = 0.0d0

    call radial(l, n_r, r_grid, n_basis, analytic_basis)

    write (filename, "(i1, a)") l, ".txt"
    open (unit=10, file=filename, action="write")
    do ii = 1, n_r
      write (10, *) r_grid(ii), " ", analytic_basis(ii, :)
    end do
    close (10)

  end do

contains

  subroutine radial (l, n_r, r_grid, n_basis, analytic_basis)
    integer , intent(in) :: l, n_r, n_basis
    double precision , intent(in) :: r_grid(n_r)
    double precision , intent(out) :: analytic_basis(n_r, n_basis)
    double precision :: rho(n_r)
    double precision :: norm(n_basis)
    integer :: n

    !
    do n = 1, n_basis
      norm(n) = sqrt(((2.0d0 / dble(n + l)) ** 3) * gamma(dble(n)) / &
          (2.0d0 * dble(n + l) * gamma(dble(n + 2*l + 1))))
    end do

    !
    do n = 1, n_basis
      rho(:) = r_grid(:) * 2.0d0 / dble(n+l)
      do ii = 1, n_r
        analytic_basis(ii, n) = (rho(ii) ** l) * r_grid(ii) &
            * exp(-rho(ii) / 2.0d0) &
            * laguerre(n-1, 2*l + 1, rho(ii))
      end do

      analytic_basis(:, n) = analytic_basis(:, n) * norm(n)
    end do

  end subroutine radial

  recursive function laguerre (n, t, x) result (lag)
    integer , intent(in) :: n, t
    double precision , intent(in) :: x
    double precision :: lag

    if (n == 0) then
      lag = 1.0d0
    else if (n == 1) then
      lag = 1.0d0 + dble(t) - x
    else
      lag = ((dble(2*n - 1 + t - x) * (laguerre(n-1, t, x))) &
          - (dble(n - 1 + t) * laguerre(n-2, t, x))) / dble(n)
    end if
  end function laguerre
end program analytic
