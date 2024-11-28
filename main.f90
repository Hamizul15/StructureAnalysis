program structure_analysis
    use input_manager
    use calculator_manager
    implicit none

    type(Input) :: inp
    type(CalculatorManager) :: cacl

    call inp%start()
    call cacl%calculate(inp)

end program
