cmake_minimum_required (VERSION 2.6)
enable_language(Fortran)

#find_package(Git)
#if(Git_FOUND)
#  message("Git found: ${GIT_EXECUTABLE}")
#  set(USE_GIT 1)
#else(Git_Found)
#  set(USE_GIT 0)
#endif()

project (Stat-Roll-2D)

# add the executable
add_executable (Stat-Roll-2D main.f95)