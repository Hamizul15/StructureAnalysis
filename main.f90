program structure_analysis
    use input_manager
    use calculator_manager
    implicit none

    type(Input) :: inp
    type(CalculatorManager) :: cacl
    character :: continue_option

    do
        call inp%start()
        call cacl%calculate(inp)

        continue_option = get_character("Menghitung lagi(Input Y) atau Berhenti(Input sesuatu): ")
        if((continue_option /= 'Y').and.(continue_option /= 'y')) then
            EXIT
        end if
    END DO

end program
