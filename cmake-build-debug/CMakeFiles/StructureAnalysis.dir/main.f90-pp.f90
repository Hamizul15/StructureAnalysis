# 1 "D:/Programming/Fortran/StructureAnalysis/main.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/main.f90"
program structure_analysis
    use input_manager
    use calculator_manager
    implicit none

    type(Input) :: inp
    type(CalculatorManager) :: cacl

    call inp%start()
    call cacl%calculate(inp)

end program
