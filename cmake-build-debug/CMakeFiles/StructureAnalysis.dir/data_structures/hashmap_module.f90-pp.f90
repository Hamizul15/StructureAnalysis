# 1 "D:/Programming/Fortran/StructureAnalysis/data_structures/hashmap_module.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/data_structures/hashmap_module.f90"
module hashmap_module
    implicit none
    private
    public :: HashMap, init, add, remove, contains_key, try_get_value

    integer, parameter :: hash_table_size = 128

    type :: Node
        class(*), allocatable :: key
        class(*), allocatable :: value
        type(Node), pointer :: next => null()
    end type Node

    type :: HashMap
        type(Node), pointer :: table(:)
        integer :: size
        integer :: count
    contains
        procedure :: init
        procedure :: add
        procedure :: remove
        procedure :: contains_key
        procedure :: try_get_value
    end type HashMap

contains

    ! Initialize the hash map
    subroutine init(this)
        class(HashMap), intent(inout) :: this
        integer :: i

        allocate(this%table(hash_table_size))
        this%size = hash_table_size
        this%count = 0

        ! Initialize each table entry to null
        do i = 1, this%size
            nullify(this%table(i))
        end do
    end subroutine init

    ! Compute a hash value for a given key
    function hash_function(key, size) result(hash)
        class(*), intent(in) :: key
        integer, intent(in) :: size
        integer :: hash

        select type (key)
        type is (integer)
            hash = mod(key, size)
        type is (real)
            hash = mod(int(abs(key) * 1.0e6), size)
        class default
            print *, "Unsupported key type for hashing"
            hash = -1
        end select
    end function hash_function

    ! Add a key-value pair to the hash map
    subroutine add(this, key, value)
        class(HashMap), intent(inout) :: this
        class(*), intent(in) :: key
        class(*), intent(in) :: value
        type(Node), pointer :: new_node, current
        integer :: index

        index = hash_function(key, this%size)
        if (index == -1) stop "Unsupported key type for hashing"

        ! Check if the key already exists
        current => this%table(index)
        do while (associated(current))
            if (keys_equal(current%key, key)) then
                stop "Key already exists in the hash map"
            end if
            current => current%next
        end do

        ! Add the new key-value pair
        allocate(new_node)
        allocate(new_node%key, source=key)
        allocate(new_node%value, source=value)
        new_node%next => this%table(index)
        this%table(index) => new_node
        this%count = this%count + 1
    end subroutine add

    ! Remove a key-value pair from the hash map
    subroutine remove(this, key)
        class(HashMap), intent(inout) :: this
        class(*), intent(in) :: key
        type(Node), pointer :: current, prev
        integer :: index

        index = hash_function(key, this%size)
        if (index == -1) stop "Unsupported key type for hashing"

        current => this%table(index)
        prev => null()

        do while (associated(current))
            if (keys_equal(current%key, key)) then
                if (associated(prev)) then
                    prev%next => current%next
                else
                    this%table(index) => current%next
                end if
                deallocate(current%key, current%value, current)
                this%count = this%count - 1
                return
            end if
            prev => current
            current => current%next
        end do

        print *, "Key not found in the hash map"
    end subroutine remove

    ! Check if the hash map contains a key
    pure function contains_key(this, key) result(found)
        class(HashMap), intent(in) :: this
        class(*), intent(in) :: key
        logical :: found
        type(Node), pointer :: current
        integer :: index

        index = hash_function(key, this%size)
        if (index == -1) stop "Unsupported key type for hashing"

        current => this%table(index)
        found = .false.

        do while (associated(current))
            if (keys_equal(current%key, key)) then
                found = .true.
                return
            end if
            current => current%next
        end do
    end function contains_key

    ! Try to get a value by key
    subroutine try_get_value(this, key, value, found)
        class(HashMap), intent(in) :: this
        class(*), intent(in) :: key
        class(*), allocatable :: value
        logical, intent(out) :: found
        type(Node), pointer :: current
        integer :: index

        index = hash_function(key, this%size)
        if (index == -1) stop "Unsupported key type for hashing"

        current => this%table(index)
        found = .false.

        do while (associated(current))
            if (keys_equal(current%key, key)) then
                allocate(value, source=current%value)
                found = .true.
                return
            end if
            current => current%next
        end do
    end subroutine try_get_value

    ! Compare two keys for equality
    pure function keys_equal(key1, key2) result(equal)
        class(*), intent(in) :: key1, key2
        logical :: equal

        select type (key1)
        type is (integer)
            select type (key2)
            type is (integer)
                equal = key1 == key2
            class default
                equal = .false.
            end select
        type is (real)
            select type (key2)
            type is (real)
                equal = abs(key1 - key2) < 1.0e-6
            class default
                equal = .false.
            end select
        class default
            equal = .false.
        end select
    end function keys_equal

end module hashmap_module
