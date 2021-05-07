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
  pure subroutine radial_basis (l, alpha, n_r, r_grid, n_basis, basis)
    integer , intent(in) :: l, n_r, n_basis
    real , intent(in) :: alpha
    real , intent(in) :: r_grid(n_r)
    real , intent(out) :: basis(n_r, n_basis)
    real :: norm(n_basis)
    real :: alpha_grid(n_r)
    integer :: kk

    ! in-lined array since r_grid(:) on its own is never used
    alpha_grid(:) = alpha * r_grid(:)

    ! recurrence relation for basis functions
    basis(:, 1) = ((2.0 * alpha_grid(:)) ** (l + 1)) * &
        exp(-alpha_grid(:))

    basis(:, 2) = 2.0 * (l + 1 - alpha_grid(:)) * basis(:, 1)

    do kk = 3, n_basis
      basis(:, kk) = ((2.0 * (kk - 1 + l - alpha_grid(:)) * basis(:, kk-1)) &
          - (kk + (2 * l) - 1) * basis(:, kk-2)) / real(kk - 1)
    end do

    ! recurrence relation for basis normalisation constants
    norm(1) = sqrt(alpha / real((l + 1) * gamma((2 * l) + 2.0)))

    do kk = 2, n_basis
      norm(kk) = norm(kk-1) * sqrt(real(kk * (kk + l)) / &
          real((kk + l + 1) * (kk + (2 * l) + 1)))
    end do

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
  ! these matrix elements.
  subroutine overlap_matrix(l, m, n_basis, B)
    integer , intent(in) :: l, m, n_basis
    real , intent(out) :: B(n_basis, n_basis)
    integer :: kk

    ! initialise overlap matrix to zero
    B(:, :) = 0.0

    ! determine tri-diagonal overlap matrix elements
    do kk = 1, n_basis-1
      B(kk, kk) = 1.0

      B(kk, kk+1) = - 0.5 * sqrt(1 - &
          (real(l * (l + 1)) / real((kk + l) * (kk + l + 1))))

      B(kk+1, kk) = B(kk, kk+1)
    end do

    ! last term (not covered by loop)
    B(n_basis, n_basis) = 1.0

  end subroutine overlap_matrix

  ! kinetic_matrix
  !
  ! < phi_{k', l, m} | K | phi_{k, l, m} >
  !
  ! Kinetic matrix elements for given l, m, alpha.
  ! We can restrict our attention to considering fixed l and m, since the matrix
  ! elements are zero when l' /= l or where m' /= m.
  subroutine kinetic_matrix(l, m, alpha, n_basis, K)
    integer , intent(in) :: l, m, n_basis
    real , intent(in) :: alpha
    real , intent(out) :: K(n_basis, n_basis)
    integer :: kk

    ! initialise kinetic matrix to zero
    K(:, :) = 0.0

    ! determine tri-diagonal kinetic matrix elements
    do kk = 1, n_basis-1
      K(kk, kk) = 0.5 * (alpha ** 2)

      K(kk, kk+1) = (alpha ** 2) * 0.25 * sqrt(1 - &
          (real(l * (l + 1)) / real((kk + l) * (kk + l + 1))))

      K(kk+1, kk) = K(kk, kk+1)
    end do

    ! last term (not covered by loop)
    K(n_basis, n_basis) = 0.5 * (alpha ** 2)

  end subroutine kinetic_matrix

  ! coulomb_matrix
  !
  ! < phi_{k', l, m} | 1/r | phi_{k, l, m} >
  !
  ! Coulomb matrix elements for given l, m, alpha.
  ! We can restrict our attention to considering fixed l and m, since the matrix
  ! elements are zero when l' /= l or where m' /= m.
  subroutine coulomb_matrix(l, m, alpha, n_basis, V)
    integer , intent(in) :: l, m, n_basis
    real , intent(in) :: alpha
    real , intent(out) :: V(n_basis, n_basis)
    integer :: kk

    ! initialise coulomb matrix to zero
    V(:, :) = 0.0

    ! determine diagonal coulomb matrix elements
    do kk = 1, n_basis
      V(kk, kk) = alpha / (kk + l)
    end do

  end subroutine coulomb_matrix

  ! hydrogenic_matrices
  !
  ! Yields overlap, kinetic, potential and Hamiltonian matrices for given l, m,
  ! alpha, z (atomic charge); that is: B, K, V, H.
  ! We can restrict our attention to considering fixed l and m, since the matrix
  ! elements are zero when l' /= l or where m' /= m.
  subroutine hydrogenic_matrices(l, m, alpha, z, n_basis, B, K, V, H)
    integer , intent(in) :: l, m, z, n_basis
    real , intent(in) :: alpha
    real , intent(out) :: B(n_basis, n_basis), K(n_basis, n_basis), &
        V(n_basis, n_basis), H(n_basis, n_basis)

    call overlap_matrix(l, m, n_basis, B)

    call kinetic_matrix(l, m, alpha, n_basis, K)

    call coulomb_matrix(l, m, alpha, n_basis, V)
    V(:, :) = - real(z) * V(:, :)

    H(:, :) = K(:, :) + V(:, :)

  end subroutine hydrogenic_matrices

end module laguerre
