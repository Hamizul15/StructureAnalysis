# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

file(MAKE_DIRECTORY
  "D:/Programming/Fortran/StructureAnalysis/cmake-build-debug/_deps/stdlib-src"
  "D:/Programming/Fortran/StructureAnalysis/cmake-build-debug/_deps/stdlib-build"
  "D:/Programming/Fortran/StructureAnalysis/cmake-build-debug/_deps/stdlib-subbuild/stdlib-populate-prefix"
  "D:/Programming/Fortran/StructureAnalysis/cmake-build-debug/_deps/stdlib-subbuild/stdlib-populate-prefix/tmp"
  "D:/Programming/Fortran/StructureAnalysis/cmake-build-debug/_deps/stdlib-subbuild/stdlib-populate-prefix/src/stdlib-populate-stamp"
  "D:/Programming/Fortran/StructureAnalysis/cmake-build-debug/_deps/stdlib-subbuild/stdlib-populate-prefix/src"
  "D:/Programming/Fortran/StructureAnalysis/cmake-build-debug/_deps/stdlib-subbuild/stdlib-populate-prefix/src/stdlib-populate-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "D:/Programming/Fortran/StructureAnalysis/cmake-build-debug/_deps/stdlib-subbuild/stdlib-populate-prefix/src/stdlib-populate-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "D:/Programming/Fortran/StructureAnalysis/cmake-build-debug/_deps/stdlib-subbuild/stdlib-populate-prefix/src/stdlib-populate-stamp${cfgdir}") # cfgdir has leading slash
endif()
