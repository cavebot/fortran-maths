# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.30

# Note that incremental build could trigger a call to cmake_copy_f90_mod on each re-build
CMakeFiles/openblas_example.dir/include/matrix_operations.f90.o.provides.build: CMakeFiles/openblas_example.dir/matrix_operations.mod.stamp
CMakeFiles/openblas_example.dir/matrix_operations.mod.stamp: CMakeFiles/openblas_example.dir/include/matrix_operations.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod matrix_operations.mod CMakeFiles/openblas_example.dir/matrix_operations.mod.stamp GNU
CMakeFiles/openblas_example.dir/include/matrix_operations.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch CMakeFiles/openblas_example.dir/include/matrix_operations.f90.o.provides.build
CMakeFiles/openblas_example.dir/build: CMakeFiles/openblas_example.dir/include/matrix_operations.f90.o.provides.build
CMakeFiles/openblas_example.dir/src/main.f90.o: CMakeFiles/openblas_example.dir/matrix_operations.mod.stamp
CMakeFiles/openblas_example.dir/src/utils.f90.o.provides.build: CMakeFiles/openblas_example.dir/utils.mod.stamp
CMakeFiles/openblas_example.dir/utils.mod.stamp: CMakeFiles/openblas_example.dir/src/utils.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod utils.mod CMakeFiles/openblas_example.dir/utils.mod.stamp GNU
CMakeFiles/openblas_example.dir/src/utils.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch CMakeFiles/openblas_example.dir/src/utils.f90.o.provides.build
CMakeFiles/openblas_example.dir/build: CMakeFiles/openblas_example.dir/src/utils.f90.o.provides.build
