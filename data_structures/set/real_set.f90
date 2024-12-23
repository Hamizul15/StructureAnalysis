module real_set
    implicit none

    integer, parameter :: initial_size = 10

    type RealSet
        private
        integer :: size = 0
        real, allocatable :: arr(:)

    contains
        private
        procedure :: resize, initialize

        procedure, public :: add_real_to_set, get_real_from_set, get_size, clear_real_set, sort_real_set!, remove
    end type RealSet

contains

    ! Initialize the array with a given size
    subroutine initialize(this)
        class(RealSet), intent(inout) :: this

        if (.not.allocated( this%arr)) then
            allocate(this%arr(initial_size))
            this%size = 0
        end if
    end subroutine initialize

    subroutine add_real_to_set(this, lo)
        class(RealSet), intent(inout) :: this
        real :: lo
        integer :: i

        if (this%size == 0) then
            call this%initialize()
        end if

        if (this%size == size(this%arr)) then
            ! Double the size of the array if it's full
            call this%resize(size(this%arr) * 2)
        end if

        do i = 1, this%size
            if(this%arr(i) == lo) return
        end do

        this%size = this%size + 1
        this%arr(this%size) = lo
    end subroutine add_real_to_set

    ! Resize the array to a new size
    subroutine resize(this, new_size)
        class(RealSet), intent(inout) :: this
        integer, intent(in) :: new_size
        real, allocatable :: temp(:)

        allocate(temp(new_size))
        temp(1:this%size) = this%arr(1:this%size)  ! Copy existing elements to temp
        deallocate(this%arr)              ! Deallocate old array
        allocate(this%arr(new_size))      ! Reallocate the array with the new size
        this%arr = temp                   ! Copy back the elements to the new array
        deallocate(temp)             ! Deallocate temporary array
    end subroutine resize

    subroutine clear_real_set(this)
        class(RealSet), intent(inout) :: this

        this%size = 0
        deallocate(this%arr)
    end subroutine clear_real_set

    ! Get the current size of the array
    function get_size(this) result(s)
        class(RealSet), intent(inout) :: this
        integer :: s

        s = this%size
    end function get_size

    function get_real_from_set(this, index) result(lo)
        class(RealSet), intent(inout) :: this
        integer, intent(in) :: index
        real :: lo

        if (index > 0 .and. index <= this%size) then
            lo = this%arr(index)
        else
            stop "Index out of bounds"
        end if
    end function get_real_from_set

    subroutine sort_real_set(this)
        class(RealSet), intent(inout) :: this
        integer :: i, j
        real :: temp

        if (this%size <= 1) return  ! No need to sort if the size is 0 or 1

        do i = 1, this%size - 1
            do j = 1, this%size - i
                if (this%arr(j) > this%arr(j + 1)) then
                    temp = this%arr(j)
                    this%arr(j) = this%arr(j + 1)
                    this%arr(j + 1) = temp
                end if
            end do
        end do
    end subroutine sort_real_set

end module real_set
