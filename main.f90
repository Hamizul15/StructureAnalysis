program structure_analysis
    use supports_module
    implicit none

    real :: test
    class(Support), allocatable :: pin_, roller_

    allocate(Pin :: pin_)
    allocate(Roller :: roller_)



    print *, roller_%get_number_of_reaction()
end program
