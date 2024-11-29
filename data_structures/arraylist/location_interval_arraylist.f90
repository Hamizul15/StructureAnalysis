module location_interval_arraylist
    use location_interval_module
    implicit none

    integer, parameter :: initial_size = 10

    type LocationIntervalArrayList
        private
        integer :: size = 0
        type(LocationInterval), allocatable :: arr(:)

    contains
        private
        procedure :: resize, initialize

        procedure, public :: add_location_interval, get_location_lnterval, get_size, clear_intervals
    end type LocationIntervalArrayList

contains

    ! Initialize the array with a given size
    subroutine initialize(this)
        class(LocationIntervalArrayList), intent(inout) :: this

        if (.not.allocated( this%arr)) then
            allocate(this%arr(initial_size))
            this%size = 0
        end if
    end subroutine initialize

    subroutine add_location_interval(this, lo)
        class(LocationIntervalArrayList), intent(inout) :: this
        type(LocationInterval), intent(in) :: lo

        if (this%size == 0) then
            call this%initialize()
        end if

        if (this%size == size(this%arr)) then
            ! Double the size of the array if it's full
            call this%resize(size(this%arr) * 2)
        end if

        this%size = this%size + 1
        this%arr(this%size) = lo
    end subroutine add_location_interval

    ! Remove a person (find and shift remaining elements)
    !subroutine remove(this, person)
    !class(LocationIntervalHashMap), intent(inout) :: this
    !type(LocationInterval), intent(in) :: LocationInterval
    !integer :: i, found

    !found = 0
    !do i = 1, size
    !    if (this%arr(i)% arr(i)%name == person%name .and. arr(i)%age == person%age) then
    !        found = 1
    !exit
    !    end if
    !end do

    !if (found == 1) then
    ! Shift elements after the removed person
    !    do i = i, size-1
    !        arr(i) = arr(i+1)
    !    end do
    !    size = size - 1
    !else
    !    print *, "Person not found"
    !end if
    !end subroutine remove

    ! Resize the array to a new size
    subroutine resize(this, new_size)
        class(LocationIntervalArrayList), intent(inout) :: this
        integer, intent(in) :: new_size
        type(LocationInterval), allocatable :: temp(:)

        allocate(temp(new_size))
        temp(1:this%size) = this%arr(1:this%size)  ! Copy existing elements to temp
        deallocate(this%arr)              ! Deallocate old array
        allocate(this%arr(new_size))      ! Reallocate the array with the new size
        this%arr = temp                   ! Copy back the elements to the new array
        deallocate(temp)             ! Deallocate temporary array
    end subroutine resize

    subroutine clear_intervals(this)
        class(LocationIntervalArrayList), intent(inout) :: this

        this%size = 0
        deallocate(this%arr)
    end subroutine clear_intervals

    ! Get the current size of the array
    function get_size(this) result(s)
        class(LocationIntervalArrayList), intent(inout) :: this
        integer :: s

        s = this%size
    end function get_size

    function get_location_lnterval(this, index) result(lo)
        class(LocationIntervalArrayList), intent(inout) :: this
        integer, intent(in) :: index
        type(LocationInterval) :: lo

        if (index > 0 .and. index <= this%size) then
            lo = this%arr(index)
        else
            stop "Index out of bounds"
        end if
    end function get_location_lnterval


end module location_interval_arraylist
