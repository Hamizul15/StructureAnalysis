program structure_analysis
    use input_manager
    implicit none

    real :: test
    type(Input) :: input_

    call input_%set_length(10.0)

    test = input_%get_length();

    print *, test
end program
