module load_arraylist
    use loads_module
    implicit none

    integer, parameter :: initial_size = 10

    type LoadArrayList
        private
        integer :: size = 0
        type(Load), allocatable :: arr(:)

        contains
        private
        procedure :: resize, initialize

        procedure, public :: add, get, get_size !, remove
    end type LoadArrayList

contains

    ! Initialize the array with a given size
    subroutine initialize(this)
        class(LoadArrayList), intent(inout) :: this

        if (.not.allocated( this%arr)) then
            allocate(this%arr(initial_size))
            this%size = 0
        end if
    end subroutine initialize

    ! Add a person to the dynamic array
    subroutine add(this, lo)
        class(LoadArrayList), intent(inout) :: this
        type(Load), intent(in) :: lo

        if (this%size == 0) then
            call this%initialize()
        end if

        if (this%size == size(this%arr)) then
            ! Double the size of the array if it's full
            call this%resize(size(this%arr) * 2)
        end if

        this%size = this%size + 1
        this%arr(this%size) = lo
    end subroutine add

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
        class(LoadArrayList), intent(inout) :: this
        integer, intent(in) :: new_size
        type(Load), allocatable :: temp(:)

        allocate(temp(new_size))
        temp(1:this%size) = this%arr(1:this%size)  ! Copy existing elements to temp
        deallocate(this%arr)              ! Deallocate old array
        allocate(this%arr(new_size))      ! Reallocate the array with the new size
        this%arr = temp                   ! Copy back the elements to the new array
        deallocate(temp)             ! Deallocate temporary array
    end subroutine resize

    ! Get the current size of the array
    function get_size(this) result(s)
        class(LoadArrayList), intent(inout) :: this
        integer :: s

        s = this%size
    end function get_size

    function get(this, index) result(lo)
        class(LoadArrayList), intent(inout) :: this
        integer, intent(in) :: index
        type(Load) :: lo

        if (index > 0 .and. index <= this%size) then
            lo = this%arr(index)
        else
            stop "Index out of bounds"
        end if
    end function get

end module load_arraylist
