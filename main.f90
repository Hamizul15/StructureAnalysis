program structure_analysis
    use input_manager
    use message_module
    use calculator_manager
    use location_manager
    use location_interval_arraylist
    implicit none

    type(Input) :: inp
    type(CalculatorManager) :: cacl
    character :: continue_option

    do
        call display_intro()
        call construct_location_manager()

        call inp%start()
        call cacl%calculate(inp)

        continue_option = get_character("Menghitung lagi(input Y) atau Berhenti(input sesuatu): ")
        if((continue_option /= 'Y').and.(continue_option /= 'y')) then
            exit
        end if

        call inp%dispose()
        call dispose_location_manager()
        print *, ""
    end do

end program
