program structure_analysis
    use input_manager
    use calculator_service
    implicit none

    type(Input) :: inp
    class(Calculator), allocatable :: cacl

    allocate(CalculotrDetermined :: cacl)

    call inp%start()
    call cacl%init(inp)

    call cacl%calculate()
end program
