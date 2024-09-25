module utils
  implicit none
contains

  subroutine print_matrix(A)
    real(8), dimension(:,:), intent(in) :: A
    integer :: i, j
    integer :: num_cols
    character(len=20) :: fmt  ! Format for printing
    
    num_cols = size(A, 2)  ! Number of columns in the matrix
    fmt = '(20F8.4)'  ! Format to print each element with 4 decimal places

    print *, "Matrix:"
    do i = 1, size(A, 1)
      write(*, fmt) A(i, :)  ! Print each row using the defined format
    end do
  end subroutine print_matrix

end module utils
