# 1 "D:/Programming/Fortran/StructureAnalysis/data_structures/set/support_set.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/data_structures/set/support_set.f90"
module support_set
    use supports_module
    implicit none

    integer, parameter :: initial_size = 10

    type SupportSet
        private
        integer :: size = 0
        type(Support), allocatable :: arr(:)

    contains
        private
        procedure :: resize, initialize

        procedure, public :: add_support_to_set
        procedure, public :: get_support_from_set
        procedure, public :: get_size
        procedure, public :: is_location_occupied
        procedure, public :: clear_support_set
        procedure, public :: sum_of_number_of_reactions
        !procedure, public :: remove
    end type SupportSet

contains

    ! Initialize the array with a given size
    subroutine initialize(this)
        class(SupportSet), intent(inout) :: this

        if (.not.allocated( this%arr)) then
            allocate(this%arr(initial_size))
            this%size = 0
        end if
    end subroutine initialize

    subroutine add_support_to_set(this, lo)
        class(SupportSet), intent(inout) :: this
        type(Support), intent(in) :: lo
        type(Support) :: current_sup
        integer :: i

        if (this%size == 0) then
            call this%initialize()
        end if

        if (this%size == size(this%arr)) then
            ! Double the size of the array if it's full
            call this%resize(size(this%arr) * 2)
        end if

        do i = 1, this%size
            current_sup = this%arr(i)
            if(current_sup%get_location() == lo%get_location()) then
                print *, "Location has been occupied"
                return
            end if
        end do

        this%size = this%size + 1
        this%arr(this%size) = lo
    end subroutine add_support_to_set

    ! Resize the array to a new size
    subroutine resize(this, new_size)
        class(SupportSet), intent(inout) :: this
        integer, intent(in) :: new_size
        type(Support), allocatable :: temp(:)

        allocate(temp(new_size))
        temp(1:this%size) = this%arr(1:this%size)  ! Copy existing elements to temp
        deallocate(this%arr)              ! Deallocate old array
        allocate(this%arr(new_size))      ! Reallocate the array with the new size
        this%arr = temp                   ! Copy back the elements to the new array
        deallocate(temp)             ! Deallocate temporary array
    end subroutine resize

    subroutine clear_support_set(this)
        class(SupportSet), intent(inout) :: this

        this%size = 0
        deallocate(this%arr)
    end subroutine clear_support_set

    ! Get the current size of the array
    function get_size(this) result(s)
        class(SupportSet), intent(inout) :: this
        integer :: s

        s = this%size
    end function get_size

    function get_support_from_set(this, index) result(lo)
        class(SupportSet), intent(inout) :: this
        integer, intent(in) :: index
        type(Support) :: lo

        if (index > 0 .and. index <= this%size) then
            lo = this%arr(index)
        else
            stop "Index out of bounds"
        end if
    end function get_support_from_set

    function is_location_occupied(this, location) result(lo)
        class(SupportSet), intent(inout) :: this
        real, intent(in) :: location
        type(Support) :: current_sup
        logical :: lo
        integer :: i

        lo = .false.
        do i = 1, this%size
            current_sup = this%arr(i)
            if(current_sup%get_location() == location) lo = .true.
        end do
    end function is_location_occupied

    function sum_of_number_of_reactions(this) result(xxx)
        class(SupportSet), intent(inout) :: this
        integer :: xxx, i
        type(Support) :: lo

        xxx = 0
        do i = 1, this%size
            lo = this%arr(i)
            xxx = xxx + lo%get_number_of_reaction()
        end do

    end function sum_of_number_of_reactions

end module support_set
