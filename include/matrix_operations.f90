module matrix_operations
  implicit none
  contains

  subroutine matrix_multiply_blas(A, B, C)
    real(8), dimension(:,:), intent(in) :: A, B
    real(8), dimension(size(A,1), size(B,2)), intent(out) :: C
    real(8) :: alpha, beta
    integer :: m, n, k, lda, ldb, ldc

    alpha = 1.0d0
    beta = 0.0d0
    m = size(A, 1)
    n = size(B, 2)
    k = size(A, 2)
    lda = m
    ldb = k
    ldc = m

    ! Call DGEMM from BLAS to perform matrix multiplication
    call dgemm('N', 'N', m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
  end subroutine matrix_multiply_blas


  function generate_random_matrix(n) result(B)
    integer, intent(in) :: n 
    real(8), dimension(n,n) :: A, B
    integer(4) :: i, j, size
    integer, dimension(8) :: seed  ! Seed array

    call random_seed(put=seed)
    call random_number(B)

  end function generate_random_matrix


end module matrix_operations
