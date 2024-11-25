program structure_analysis
    use load_hashmap
    use support_hashmap
    implicit none

    type(SupportHashMap) :: map
    class(Support), allocatable :: test1, test2, value
    integer :: i

    allocate(Pin :: test1)
    allocate(Roller :: test2)

    ! Initialize the hash map
    call map%init()

    call test1%set_location(12.0)
    call test2%set_location(10.0)

    ! Insert some key-value pairs
    call map%insert(12.0, test1)
    call map%insert(10.0, test2)

    call map%search(12.0, value)

    print *, value%get_number_of_reaction()
end program
