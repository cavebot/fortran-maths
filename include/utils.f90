module utils
  use, intrinsic :: iso_c_binding
  implicit none

  ! Interface for the C function gethostname
  interface
     function gethostname(name, namelen) bind(c, name="gethostname")
        use, intrinsic :: iso_c_binding
        character(kind=c_char), dimension(*), intent(out) :: name
        integer, value :: namelen
        integer :: gethostname
     end function gethostname
  end interface

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
