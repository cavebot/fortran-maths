cmake_minimum_required(VERSION 3.10)
project(FortranMatrixOps C Fortran)

# Set module output directory for Fortran .mod files
set(MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
include_directories(${MODULE_DIRECTORY})

set(OPENBLAS_DIR "/usr/lib/x86_64-linux-gnu/openblas-pthread")
set(OPENBLAS_LIBRARIES ${OPENBLAS_DIR}/libopenblas.so)

find_package(MPI REQUIRED)
include_directories(${MPI_Fortran_INCLUDE_PATH})


# enable_language(Fortran)


# Set Fortran compiler flags for MPI
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${MPI_Fortran_FLAGS}")

set(COMMON_SOURCES
    ${CMAKE_SOURCE_DIR}/include/matrix_operations.f90
    ${CMAKE_SOURCE_DIR}/include/utils.f90
)

# Function to create executable that uses BLAS and common sources
function(create_executable target_name source_file)
    add_executable(${target_name} ${source_file} ${COMMON_SOURCES})
    set_target_properties(${target_name} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
    
    # Link the target with the OpenBLAS and LAPACK libraries
    target_link_libraries(${target_name} PRIVATE ${OPENBLAS_LIBRARIES})
endfunction()

# create_executable(matrix_multiply.exe ${CMAKE_SOURCE_DIR}/src/matrix_multiply.f90)
# create_executable(diagonalization.exe ${CMAKE_SOURCE_DIR}/src/diagonalization.f90)

add_executable(hpc_hello.exe ${CMAKE_SOURCE_DIR}/src/hpc_hello.f90)
set_target_properties(hpc_hello.exe PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
target_link_libraries(hpc_hello.exe ${MPI_Fortran_LIBRARIES})