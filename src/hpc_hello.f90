PROGRAM mpi_parallel_sum
   USE mpi
   USE, intrinsic :: iso_c_binding
   implicit none

   integer :: rank, size, ierr
   integer :: n, i, local_n
   real, ALLOCATABLE :: array(:), local_array(:)
   real :: local_sum, global_sum
   character(len=100), target :: hostname
   integer :: hostname_len
   integer :: status

   ! Declare the interface for the gethostname C function
   interface
      function gethostname(name, namelen) bind(C, name="gethostname")
         use, intrinsic :: iso_c_binding
         character(kind=c_char), dimension(*), intent(out) :: name
         integer, value :: namelen
         integer :: gethostname
      end function gethostname
   end interface

   call MPI_INIT(ierr)
   call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
   call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

   n = 1000

   IF (rank == 0) THEN
      ALLOCATE(array(n))
      array = 1.0
   END IF

   ! Determine the size of the portion for each process
   local_n = n / size
   IF (MOD(n, size) /= 0 .AND. rank == 0) THEN
      WRITE(*,*) 'Array size is not divisible by the number of processes.'
      call MPI_ABORT(MPI_COMM_WORLD, 1, ierr)
   END IF

   ALLOCATE(local_array(local_n))

   ! Scatter the array to all processes
   call MPI_SCATTER(array, local_n, MPI_REAL, local_array, local_n, MPI_REAL, 0, MPI_COMM_WORLD, ierr)

   ! Each process computes the sum of its portion
   local_sum = 0.0
   DO i = 1, local_n
      local_sum = local_sum + local_array(i)
   END DO

   ! Get the hostname
   hostname_len = 100
   status = gethostname(hostname(1:hostname_len), hostname_len)

   ! Print formatted output for each rank
   WRITE(*,'(A,I2,A,I2,A,A)') 'Hello world: rank ', rank, ' of ', size, ' running on node: ', trim(hostname)

   call MPI_REDUCE(local_sum, global_sum, 1, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
   call MPI_BARRIER(MPI_COMM_WORLD, ierr)

   IF (rank == 0) THEN
      WRITE(*,'(A,F10.3)') 'Total sum of the array is: ', global_sum
   END IF

   call MPI_FINALIZE(ierr)
END PROGRAM mpi_parallel_sum
