program structure_analysis
    use loads_module
    implicit none

    class(Load), allocatable :: point1

    allocate(Point::point1)

    call point1%set_start_location(5.0)

    print *, point1%get_start_location()
end program
