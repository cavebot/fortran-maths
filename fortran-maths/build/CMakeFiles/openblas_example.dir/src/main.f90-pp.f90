# 1 "/home/adam/Projects/fortran-maths/src/main.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/adam/Projects/fortran-maths/build//"
# 1 "/home/adam/Projects/fortran-maths/src/main.f90"
program main
  use matrix_operations
  use utils
  implicit none

  integer, parameter :: n = 4  ! Size of the matrix (4x4)
  real(8), dimension(n, n) :: A, B, C

  ! Seed the random number generator
  call random_seed()

  ! Generate random matrices A and B
  call random_number(A)
  call random_number(B)

  ! Print the generated matrices
  print *, "Matrix A:"
  call print_matrix(A)
  print *, "Matrix B:"
  call print_matrix(B)

  ! Perform matrix multiplication
  call matrix_multiply_blas(A, B, C)

  ! Print the result
  print *, "Result of A * B:"
  call print_matrix(C)

end program main
