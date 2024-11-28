! Created by hamiz on 11/27/2024.
module resultload_arraylist
    implicit none

    integer, parameter :: initial_size = 10

    type :: ResultLoad
        private
        real :: loc, load

    contains

        procedure :: set_location, set_load
        procedure :: get_location, get_load

    end type ResultLoad

    type ResultLoadArrayList
        private
        integer :: size = 0
        type(ResultLoad), allocatable :: arr(:)

    contains
        private
        procedure :: resize, initialize

        procedure, public :: add_resultload, get_resultload, get_size !, remove
    end type ResultLoadArrayList

contains

    function new_resultload(location, load) result(xxx)
        type(ResultLoad) :: xxx
        real :: location, load

        call xxx%set_location(location)
        call xxx%set_load(load)
    end function new_resultload

    !ResultLoad
    subroutine set_location(this, location)
        class(ResultLoad), intent(inout) :: this
        real :: location

        this%loc = location
    end subroutine set_location

    subroutine set_load(this, load)
        class(ResultLoad), intent(inout) :: this
        real :: load

        this%load = load
    end subroutine set_load


    function get_location(this) result(loc_)
        class(ResultLoad), intent(in) :: this
        real :: loc_

        loc_ = this%loc;
    end function get_location

    function get_load(this) result(lo)
        class(ResultLoad), intent(in) :: this
        real :: lo

        lo = this%load;
    end function get_load



    !ResultLoadArrayList
    ! Initialize the array with a given size
    subroutine initialize(this)
        class(ResultLoadArrayList), intent(inout) :: this

        if (.not.allocated( this%arr)) then
            allocate(this%arr(initial_size))
            this%size = 0
        end if
    end subroutine initialize

    subroutine add_resultload(this, lo)
        class(ResultLoadArrayList), intent(inout) :: this
        type(ResultLoad), intent(in) :: lo

        if (this%size == 0) then
            call this%initialize()
        end if

        if (this%size == size(this%arr)) then
            ! Double the size of the array if it's full
            call this%resize(size(this%arr) * 2)
        end if

        this%size = this%size + 1
        this%arr(this%size) = lo
    end subroutine add_resultload

    ! Remove a person (find and shift remaining elements)
    !subroutine remove(this, person)
    !class(LoadHashMap), intent(inout) :: this
    !type(Load), intent(in) :: load
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
        class(ResultLoadArrayList), intent(inout) :: this
        integer, intent(in) :: new_size
        type(ResultLoad), allocatable :: temp(:)

        allocate(temp(new_size))
        temp(1:this%size) = this%arr(1:this%size)  ! Copy existing elements to temp
        deallocate(this%arr)              ! Deallocate old array
        allocate(this%arr(new_size))      ! Reallocate the array with the new size
        this%arr = temp                   ! Copy back the elements to the new array
        deallocate(temp)             ! Deallocate temporary array
    end subroutine resize

    ! Get the current size of the array
    function get_size(this) result(s)
        class(ResultLoadArrayList), intent(inout) :: this
        integer :: s

        s = this%size
    end function get_size

    function get_resultload(this, index) result(lo)
        class(ResultLoadArrayList), intent(inout) :: this
        integer, intent(in) :: index
        type(ResultLoad) :: lo

        if (index > 0 .and. index <= this%size) then
            lo = this%arr(index)
        else
            stop "Index out of bounds"
        end if
    end function get_resultload
    
end module resultload_arraylist