program mpi_parallel_sum
   use mpi
   use utils
   implicit none

   integer :: rank, size, ierr
   integer :: n, i, local_n
   real, allocatable :: array(:), local_array(:)
   real :: local_sum, global_sum
   character(len=100), target :: hostname
   integer :: hostname_len
   integer :: status

   call mpi_init(ierr)
   call mpi_comm_rank(mpi_comm_world, rank, ierr)
   call mpi_comm_size(mpi_comm_world, size, ierr)

   n = 160000

   if (rank == 0) then
      allocate(array(n))
      array = 1.0
   end if

   ! Determine the size of the portion for each process
   local_n = n / size
   if (mod(n, size) /= 0 .and. rank == 0) then
      write(*,*) 'Array size is not divisible by the number of processes.'
      call mpi_abort(mpi_comm_world, 1, ierr)
   end if

   allocate(local_array(local_n))

   ! Scatter the array to all processes
   call mpi_scatter(array, local_n, mpi_real, local_array, local_n, mpi_real, 0, mpi_comm_world, ierr)

   ! Each process computes the sum of its portion
   local_sum = 0.0
   do i = 1, local_n
      local_sum = local_sum + local_array(i)
   end do

   write(*,*) local_sum

   ! Get the hostname
   hostname_len = 100
   status = gethostname(hostname(1:hostname_len), hostname_len)

   ! Print formatted output for each rank
   write(*,'(A,I2,A,I2,A,A)') 'Hello world: rank ', rank, ' of ', size, ' running on node: ', trim(hostname)

   call mpi_reduce(local_sum, global_sum, 1, mpi_real, mpi_sum, 0, mpi_comm_world, ierr)
   ! call mpi_barrier(mpi_comm_world, ierr)

   if (rank == 0) then
      write(*,'(A,F10.3)') 'Total sum of the array is: ', global_sum
   end if

   call mpi_finalize(ierr)
end program mpi_parallel_sum
