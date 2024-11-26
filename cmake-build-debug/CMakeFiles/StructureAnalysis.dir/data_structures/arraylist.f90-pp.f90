# 1 "D:/Programming/Fortran/StructureAnalysis/data_structures/arraylist.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/data_structures/arraylist.f90"
module generic_dynamic_array_list
    implicit none
    private
    public :: array_list, initialize_list, add_item, get_item, size, delete_item

    type :: array_list
        class(*), allocatable :: elements(:)   ! Polymorphic array to hold list elements
    end type array_list

contains

    ! Initialize the list with zero elements
    subroutine initialize_list(list)
        type(array_list), intent(inout) :: list
        allocate(list%elements(0))  ! Allocate an array with 0 elements initially
    end subroutine initialize_list

    ! Add an item to the list
    subroutine add_item(list, item)
        type(array_list), intent(inout) :: list
        class(*), intent(in) :: item
        class(*), allocatable :: temp(:)
        integer :: n

        ! Get the current number of elements in the list
        n = size(list%elements)

        ! Allocate a temporary array with the same type as list's elements
        allocate(temp(n), mold=list%elements)

        ! Copy the existing elements into the temporary array
        if (n > 0) temp = list%elements

        ! Deallocate old array and allocate a new one with 1 more element
        deallocate(list%elements)
        allocate(list%elements(n + 1), mold=temp)

        ! Copy the old elements back into the list
        if (n > 0) list%elements(1:n) = temp

        ! Insert the new item at the last position
        list%elements(n + 1) = item
    end subroutine add_item

    ! Get an item from the list at a specific index
    subroutine get_item(list, index, item)
        type(array_list), intent(in) :: list
        integer, intent(in) :: index
        class(*), allocatable, intent(out) :: item

        ! Check if index is within valid bounds
        if (index < 1 .or. index > size(list%elements)) then
            print *, "Error: Index out of bounds"
            stop
        end if

        ! Allocate item and copy the element at the specified index
        allocate(item, source=list%elements(index))
    end subroutine get_item

    ! Delete an item from the list at a specific index
    subroutine delete_item(list, index)
        type(array_list), intent(inout) :: list
        integer, intent(in) :: index
        class(*), allocatable :: temp(:)
        integer :: n

        ! Get the current number of elements in the list
        n = size(list%elements)

        ! Check if index is valid
        if (index < 1 .or. index > n) then
            print *, "Error: Index out of bounds"
            stop
        end if

        ! Create a temporary array to hold elements excluding the deleted one
        allocate(temp(n - 1), mold=list%elements)

        ! Copy elements before the index
        if (index > 1) temp(1:index-1) = list%elements(1:index-1)

        ! Copy elements after the index
        if (index < n) temp(index:n-1) = list%elements(index+1:n)

        ! Deallocate the old list and reallocate the list to the new size
        deallocate(list%elements)
        allocate(list%elements(n - 1), mold=temp)

        ! Copy the elements from the temporary array back to the list
        list%elements = temp
    end subroutine delete_item

    ! Get the size of the list
    integer function size(list)
        type(array_list), intent(in) :: list
        size = size(list%elements)
    end function size

end module generic_dynamic_array_list
