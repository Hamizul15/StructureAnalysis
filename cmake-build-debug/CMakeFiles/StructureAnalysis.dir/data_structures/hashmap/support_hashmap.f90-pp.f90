# 1 "D:/Programming/Fortran/StructureAnalysis/data_structures/hashmap/support_hashmap.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/data_structures/hashmap/support_hashmap.f90"
module support_hashmap
    use supports_module
    implicit none

    integer, parameter :: initial_size = 10

    type SupportHashMap
        private
        integer :: size = 0
        type(Support), allocatable :: arr(:)
        real, allocatable :: keys(:)

    contains
        private
        procedure :: resize, initialize

        procedure, public :: insert_support, get_support_by_key, get_support_by_index
        procedure, public :: get_size, get_keys, get_values, sum_of_number_of_reactions !, remove
    end type SupportHashMap

contains

    ! Initialize the array with a given size
    subroutine initialize(this)
        class(SupportHashMap), intent(inout) :: this

        if (.not.allocated( this%arr)) then
            allocate(this%arr(initial_size))
            allocate(this%keys(initial_size))
            this%size = 0
        end if
    end subroutine initialize

    subroutine insert_support(this, location, lo)
        class(SupportHashMap), intent(inout) :: this
        type(Support), intent(in) :: lo
        real :: location
        integer :: i

        if (this%size == 0) then
            call this%initialize()
        end if

        do i = 0, size(this%keys)
            if(this%keys(i) == location) then
                stop "The Key already exist"
            end if
        end do

        if (this%size == size(this%arr)) then
            ! Double the size of the array if it's full
            call this%resize(size(this%arr) * 2)
        end if

        this%size = this%size + 1
        this%arr(this%size) = lo
        this%keys(this%size) = location
    end subroutine insert_support

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
        class(SupportHashMap), intent(inout) :: this
        integer, intent(in) :: new_size
        type(Support), allocatable :: temp_arr(:)
        real, allocatable :: temp_keys(:)

        allocate(temp_arr(new_size))
        allocate(temp_keys(new_size))

        temp_arr(1:this%size) = this%arr(1:this%size)  ! Copy existing elements to temp
        temp_keys(1:this%size) = this%keys(1:this%size)  ! Copy existing elements to temp

        deallocate(this%arr)              ! Deallocate old array
        deallocate(this%keys)              ! Deallocate old array

        allocate(this%arr(new_size))      ! Reallocate the array with the new size
        allocate(this%keys(new_size))      ! Reallocate the array with the new size

        this%arr = temp_arr                   ! Copy back the elements to the new array
        this%keys = temp_keys                   ! Copy back the elements to the new array

        deallocate(temp_arr)             ! Deallocate temporary array
        deallocate(temp_keys)             ! Deallocate temporary array
    end subroutine resize

    ! Get the current size of the array
    function get_size(this) result(s)
        class(SupportHashMap), intent(inout) :: this
        integer :: s

        s = this%size
    end function get_size

    function get_keys(this) result(kuncis)
        class(SupportHashMap), intent(inout) :: this
        real, allocatable :: kuncis(:)

        kuncis = this%keys
    end function get_keys

    function get_values(this) result(the_value)
        class(SupportHashMap), intent(inout) :: this
        type(Support), allocatable :: the_value(:)

        the_value = this%arr
    end function get_values

    function sum_of_number_of_reactions(this) result(xxx)
        class(SupportHashMap), intent(inout) :: this
        integer :: xxx, i
        type(Support) :: lo

        xxx = 0
        do i = 1, this%size
            lo = this%arr(i)
            xxx = xxx + lo%get_number_of_reaction()
        end do

    end function sum_of_number_of_reactions

    function get_support_by_key(this, location) result(lo)
        class(SupportHashMap), intent(inout) :: this
        real, intent(in) :: location
        type(Support) :: lo
        integer :: i

        do i = 1, this%size
            lo = this%arr(i)
            if(lo%get_location() == location) then
                return
            end if
        end do
        stop "Location is not found"
    end function get_support_by_key

    function get_support_by_index(this, index) result(lo)
        class(SupportHashMap), intent(inout) :: this
        integer, intent(in) :: index
        type(Support) :: lo
        integer :: i

        if (index > 0 .and. index <= this%size) then
            lo = this%arr(index)
        else
            stop "Index out of bounds"
        end if
    end function get_support_by_index

end module support_hashmap
