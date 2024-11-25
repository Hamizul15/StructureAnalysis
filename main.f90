program structure_analysis
    use load_hashmap
    use support_hashmap
    implicit none

    type(SupportHashMap) :: map
    class(Support), allocatable :: test1, test2, test3, value
    integer :: i
    real :: key

    allocate(Pin :: test1)
    allocate(Roller :: test2)
    allocate(Fixed :: test3)

    ! Initialize the hash map
    call map%init()

    call test1%set_location(5.0)
    call test2%set_location(6.0)
    call test2%set_location(7.0)

    ! Insert some key-value pairs
    call map%insert(test1%get_location(), test1)
    call map%insert(test2%get_location(), test2)
    call map%insert(test3%get_location(), test2)

    allocate(value)

    key = test2%get_location()
    value = map%search(key)

    print *, key
    print *, value%get_location()
    print *, value%get_number_of_reaction()

end program
