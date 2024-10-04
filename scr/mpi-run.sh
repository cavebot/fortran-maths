# Define the hostfile path
HOSTFILE=~/Projects/fortran-maths/cluster/hosts.txt

# Define the executable path
EXECUTABLE=~/Projects/fortran-maths/hpc_hello

# Define the number of processes
NUM_PROCESSES=5

# Run the MPI program
mpirun -machinefile "$HOSTFILE" -np "$NUM_PROCESSES" "$EXECUTABLE"
