# 1 "D:/Programming/Fortran/StructureAnalysis/main.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/main.f90"
program structure_analysis
    use supports_module
    implicit none

    real :: test
    class(Support), allocatable :: pin_, roller_

    allocate(Pin :: pin_)
    allocate(Roller :: roller_)



    print *, roller_%get_number_of_reaction()
end program
