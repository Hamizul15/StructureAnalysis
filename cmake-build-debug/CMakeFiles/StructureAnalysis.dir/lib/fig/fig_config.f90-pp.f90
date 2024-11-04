# 1 "D:/Programming/Fortran/StructureAnalysis/lib/fig/fig_config.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/lib/fig/fig_config.f90"
module fig_config
   use iso_fortran_env, only: int32
   implicit none
   integer, parameter :: rgb_bit_depth = 8
   logical :: FIG_ABSOLUTE_COORDINATES = .false.
end module fig_config
