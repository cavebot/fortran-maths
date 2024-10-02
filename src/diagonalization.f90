program diagonalization
    use iso_fortran_env, only: wp => real64  ! Use double precision
    implicit none

    integer :: n, info, lwork
    real(wp), allocatable :: A(:,:), eigenvalues(:), work(:)

    ! Input the size of the matrix
    print *, "Enter the size of the matrix (<= 1000): "
    read(*,*) n

    ! Check if n is within valid range
    if (n <= 0 .or. n > 1000) then
        print *, "Matrix size must be greater than 0 and less than or equal to 1000."
        stop
    end if

    ! Allocate the symmetric matrix A and eigenvalue array
    allocate(A(n,n), eigenvalues(n))

    ! Initialize a simple symmetric matrix A
    ! Example: A = [4, 1; 1, 3]
    A(1,1) = 4.0_wp
    A(1,2) = 1.0_wp
    A(2,1) = 1.0_wp
    A(2,2) = 3.0_wp

    ! Print the matrix A
    print *, "Symmetric Matrix A:"
    print *, A

    ! Prepare for DSYEV
    lwork = -1  ! Ask for optimal work size
    call dsyev('V', 'L', n, A, n, eigenvalues, work, lwork, info)

    ! Now get the optimal size for the work array
    lwork = int(work(1))  ! The optimal size returned in work(1)
    allocate(work(lwork))  ! Allocate the work array

    ! Call DSYEV to compute eigenvalues and eigenvectors
    call dsyev('V', 'L', n, A, n, eigenvalues, work, lwork, info)

    ! Check for successful execution
    if (info == 0) then
        print *, "Eigenvalues computed successfully:"
        print *, eigenvalues
    else
        print *, "Error: DSYEV failed with info =", info
    end if

    ! Optionally print eigenvectors
    print *, "Eigenvectors (stored in A):"
    print *, A  ! The matrix A now contains the eigenvectors as columns

    ! Deallocate work array and other arrays
    deallocate(work, A, eigenvalues)

end program diagonalization
