program structure_analysis
    use input_manager
    use input_service
    implicit none

    type(Input) :: input_

    call input_%start()

    print *, input_%get_number_of_supports()
end program
