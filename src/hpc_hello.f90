PROGRAM mpi_parallel_sum
   USE mpi
   IMPLICIT NONE

   INTEGER :: rank, size, ierr
   INTEGER :: n, i, local_n, start_idx, end_idx
   REAL, ALLOCATABLE :: array(:), local_array(:)
   REAL :: local_sum, global_sum

   ! Initialize MPI
   CALL MPI_INIT(ierr)

   ! Get the rank of the current process
   CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

   ! Get the total number of processes
   CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

   ! Define the size of the array (this is the total size)
   n = 1000

   ! The master process (rank 0) will initialize the array
   IF (rank == 0) THEN
      ALLOCATE(array(n))
      ! Fill the array with some values, for example, the value 1.0
      array = 1.0
   END IF

   ! Determine the size of the portion for each process
   local_n = n / size
   IF (MOD(n, size) /= 0 .AND. rank == 0) THEN
      PRINT *, 'Array size is not divisible by the number of processes.'
      CALL MPI_ABORT(MPI_COMM_WORLD, 1, ierr)
   END IF

   ! Each process gets its own local array portion
   ALLOCATE(local_array(local_n))

   ! Scatter the array to all processes
   CALL MPI_SCATTER(array, local_n, MPI_REAL, local_array, local_n, MPI_REAL, 0, MPI_COMM_WORLD, ierr)

   ! Each process computes the sum of its portion
   local_sum = 0.0
   DO i = 1, local_n
      local_sum = local_sum + local_array(i)
   END DO

   ! Print the local sum for each process
   PRINT *, 'Process', rank, 'has local sum:', local_sum

   ! Use MPI_Reduce to sum all local sums into global_sum on the root process
   CALL MPI_REDUCE(local_sum, global_sum, 1, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

   CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

   ! The root process (rank 0) prints the result
   IF (rank == 0) THEN
      PRINT *, 'Total sum of the array is:', global_sum
   END IF

   ! Finalize MPI
   CALL MPI_FINALIZE(ierr)
  END PROGRAM mpi_parallel_sum
  
