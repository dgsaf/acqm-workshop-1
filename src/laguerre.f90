!>
module laguerre
  implicit none

contains

  ! radial_basis
  !
  ! phi_{k, l, m}(r, theta, phi) = (varphi_{k, l}(r) / r) * Y_{l, m}(theta, phi)
  ! where
  ! varphi_{k, l}(r) = sqrt(alpha * (k - 1)! / (k + l) * (k + 2*l)!)
  !                    * (2*alpha*r)^{l+1}
  !                    * exp(-alpha*r)
  !                    * L_{k - 1}^{2*l + 1}(2*alpha*r)
  ! where L_{i}^{j} are the generalised Laguerre polynomials.
  !
  ! For given l, alpha, and r_grid, yields the functions varphi_{k, l}(r) for
  ! k = 1, ..., n_basis, on the radial values specified in the grid.
  !
  ! Also returns an error code <ierr> where:
  ! - 0 indicates successful execution,
  ! - 1 indicates invalid arguments.
  pure subroutine radial_basis (l, alpha, n_r, r_grid, n_basis, basis, ierr)
    integer , intent(in) :: l, n_r, n_basis
    double precision , intent(in) :: alpha
    double precision , intent(in) :: r_grid(n_r)
    double precision , intent(out) :: basis(n_r, n_basis)
    integer , intent(out) :: ierr
    double precision :: norm(n_basis)
    double precision :: alpha_grid(n_r)
    integer :: kk

    ! check if arguments are valid
    ierr = 0

    if ((l < 0) .or. (n_basis < 1) .or. (n_r < 1)) then
      ierr = 1
      return
    end if

    ! recurrence relation for basis normalisation constants
    norm(1) = sqrt(alpha / dble((l + 1) * gamma(dble((2 * l) + 2))))

    if (n_basis >= 2) then
      do kk = 2, n_basis
        norm(kk) = norm(kk-1) * sqrt(dble((kk - 1) * (kk - 1 + l)) / &
            dble((kk + l) * (kk + (2 * l))))
      end do
    end if

    ! in-lined array since r_grid(:) on its own is never used
    alpha_grid(:) = alpha * r_grid(:)

    ! recurrence relation for basis functions
    basis(:, 1) = ((2.0d0 * alpha_grid(:)) ** (l + 1)) * &
        exp(-alpha_grid(:))

    if (n_basis >= 2) then
      basis(:, 2) = 2.0d0 * (dble(l + 1) - alpha_grid(:)) * basis(:, 1)
    end if

    if (n_basis >= 3) then
      do kk = 3, n_basis
        basis(:, kk) = &
            ((2.0d0 * (dble(kk - 1 + l) - alpha_grid(:)) * basis(:, kk-1)) &
            - dble(kk + (2 * l) - 1) * basis(:, kk-2)) / dble(kk - 1)
      end do
    end if

    ! scaling basis functions by normalisation constants
    do kk = 1, n_basis
      basis(:, kk) = basis(:, kk) * norm(kk)
    end do

  end subroutine radial_basis

  ! overlap_matrix
  !
  ! < phi_{k', l, m} | phi_{k, l, m} >
  !
  ! Overlap matrix elements for given l, m.
  ! We can restrict our attention to considering fixed l and m, since the matrix
  ! elements are zero when l' /= l or where m' /= m.
  ! Furthermore, the exponential decay variable, alpha, has no influence on
  ! these matrix elements, nor does the magnetic quantum number, m.
  !
  ! Also returns an error code <ierr> where:
  ! - 0 indicates successful execution,
  ! - 1 indicates invalid arguments.
  pure subroutine overlap_matrix(l, n_basis, B, ierr)
    integer , intent(in) :: l, n_basis
    double precision , intent(out) :: B(n_basis, n_basis)
    integer , intent(out) :: ierr
    integer :: kk

    ! check if arguments are valid
    ierr = 0

    if ((l < 0) .or. (n_basis < 1)) then
      ierr = 1
      return
    end if

    ! initialise overlap matrix to zero
    B(:, :) = 0.0d0

    ! determine tri-diagonal overlap matrix elements
    do kk = 1, n_basis-1
      B(kk, kk) = 1.0d0

      B(kk, kk+1) = - 0.5d0 * sqrt(1 - &
          (dble(l * (l + 1)) / dble((kk + l) * (kk + l + 1))))

      B(kk+1, kk) = B(kk, kk+1)
    end do

    ! last term (not covered by loop)
    B(n_basis, n_basis) = 1.0d0

  end subroutine overlap_matrix

  ! kinetic_matrix
  !
  ! < phi_{k', l, m} | K | phi_{k, l, m} >
  !
  ! Kinetic matrix elements for given l, m, alpha.
  ! We can restrict our attention to considering fixed l and m, since the matrix
  ! elements are zero when l' /= l or where m' /= m.
  ! Furthermore, the magnetic quantum number, m, has no influence on these
  ! matrix elements.
  !
  ! Also returns an error code <ierr> where:
  ! - 0 indicates successful execution,
  ! - 1 indicates invalid arguments.
  pure subroutine kinetic_matrix(l, alpha, n_basis, K, ierr)
    integer , intent(in) :: l, n_basis
    double precision , intent(in) :: alpha
    double precision , intent(out) :: K(n_basis, n_basis)
    integer , intent(out) :: ierr
    integer :: kk

    ! check if arguments are valid
    ierr = 0

    if ((l < 0) .or. (n_basis < 1)) then
      ierr = 1
      return
    end if

    ! initialise kinetic matrix to zero
    K(:, :) = 0.0d0

    ! determine tri-diagonal kinetic matrix elements
    do kk = 1, n_basis-1
      K(kk, kk) = 0.5d0 * (alpha ** 2)

      K(kk, kk+1) = (alpha ** 2) * 0.25d0 * sqrt(1 - &
          (dble(l * (l + 1)) / dble((kk + l) * (kk + l + 1))))

      K(kk+1, kk) = K(kk, kk+1)
    end do

    ! last term (not covered by loop)
    K(n_basis, n_basis) = 0.5d0 * (alpha ** 2)

  end subroutine kinetic_matrix

  ! coulomb_matrix
  !
  ! < phi_{k', l, m} | 1/r | phi_{k, l, m} >
  !
  ! Coulomb matrix elements for given l, m, alpha.
  ! We can restrict our attention to considering fixed l and m, since the matrix
  ! elements are zero when l' /= l or where m' /= m.
  ! Furthermore, the magnetic quantum number, m, has no influence on these
  ! matrix elements.
  !
  ! Also returns an error code <ierr> where:
  ! - 0 indicates successful execution,
  ! - 1 indicates invalid arguments.
  pure subroutine coulomb_matrix(l, alpha, n_basis, V, ierr)
    integer , intent(in) :: l, n_basis
    double precision , intent(in) :: alpha
    double precision , intent(out) :: V(n_basis, n_basis)
    integer , intent(out) :: ierr
    integer :: kk

    ! check if arguments are valid
    ierr = 0

    if ((l < 0) .or. (n_basis < 1)) then
      ierr = 1
      return
    end if

    ! initialise coulomb matrix to zero
    V(:, :) = 0.0d0

    ! determine diagonal coulomb matrix elements
    do kk = 1, n_basis
      V(kk, kk) = alpha / dble(kk + l)
    end do

  end subroutine coulomb_matrix

  ! hydrogenic_matrices
  !
  ! Yields overlap, kinetic, potential and Hamiltonian matrices for given l, m,
  ! alpha, atomic_charge; that is: B, K, V, H.
  ! We can restrict our attention to considering fixed l and m, since the matrix
  ! elements are zero when l' /= l or where m' /= m.
  ! Furthermore, the magnetic quantum number, m, has no influence on these
  ! matrix elements.
  !
  ! Also returns an error code <ierr> where:
  ! - 0 indicates successful execution,
  ! - 1 indicates invalid arguments.
  pure subroutine hydrogenic_matrices(l, alpha, atomic_charge, n_basis, B, &
      K, V, H, ierr)
    integer , intent(in) :: l, atomic_charge, n_basis
    double precision , intent(in) :: alpha
    double precision , intent(out) :: B(n_basis, n_basis), K(n_basis, n_basis), &
        V(n_basis, n_basis), H(n_basis, n_basis)
    integer , intent(out) :: ierr

    ! check if arguments are valid
    ierr = 0

    if ((l < 0) .or. (n_basis < 1)) then
      ierr = 1
      return
    end if

    ! calculate matrices
    call overlap_matrix(l, n_basis, B, ierr)

    call kinetic_matrix(l, alpha, n_basis, K, ierr)

    call coulomb_matrix(l, alpha, n_basis, V, ierr)
    V(:, :) = - dble(atomic_charge) * V(:, :)

    H(:, :) = K(:, :) + V(:, :)

  end subroutine hydrogenic_matrices

end module laguerre
