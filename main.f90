program structure_analysis
    use input_manager
    implicit none

    type(Input) :: inp

    call inp%init()
    call inp%start()

end program
