

cmake_minimum_required(VERSION 3.10)
project(OpenBLASExample Fortran)

# Set the OpenBLAS directory
set(OpenBLAS_DIR "/usr/lib/x86_64-linux-gnu/openblas-pthread/cmake/openblas")  

# Find OpenBLAS
find_package(OpenBLAS REQUIRED)

# Include directories for module files
include_directories(include)

# Specify the module sources
set(MODULE_SOURCES 
    include/matrix_operations.f90 
)

# Create the executable target
add_executable(openblas_example src/main.f90 src/utils.f90 ${MODULE_SOURCES})

# Specify where to put the executables
set_target_properties(openblas_example PROPERTIES 
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)  # Specify executable output directory

# Specify where to put the module files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)  # Specify module output directory

# Link OpenBLAS directly
target_link_libraries(openblas_example PRIVATE /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblas.so)
