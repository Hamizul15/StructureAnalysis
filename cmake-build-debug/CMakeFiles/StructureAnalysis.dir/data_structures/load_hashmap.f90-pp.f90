# 1 "D:/Programming/Fortran/StructureAnalysis/data_structures/load_hashmap.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/data_structures/load_hashmap.f90"
module load_hashmap
    use loads_module
    implicit none

    ! Define the initial size of the hash map
    integer, parameter :: INITIAL_SIZE = 100
    real, parameter :: LOAD_FACTOR_THRESHOLD = 0.75

    ! Define the hash map structure
    type :: HashMap
        private
        real, dimension(:), allocatable :: keys
        type(Load), dimension(:), allocatable :: values
        logical, dimension(:), allocatable :: occupied
        integer :: size         ! Current size of the hash map
        integer :: count        ! Number of elements stored

    contains

        procedure :: init, insert, search, resize
    end type HashMap

contains

    ! Initialize the hash map
    subroutine init(this)
        class(HashMap), intent(out) :: this
        integer :: i

        allocate(this%keys(INITIAL_SIZE))
        allocate(this%values(INITIAL_SIZE))
        allocate(this%occupied(INITIAL_SIZE))

        ! Initialize all entries to invalid
        this%keys = -1.0
        do i = 1, this%size
            call this%values(i)%set_start_location(-1.0)
            call this%values(i)%set_end_location(-1.0)
            call this%values(i)%set_start_load(-1.0)
            call this%values(i)%set_end_load(-1.0)
        end do
        this%occupied = .false.
        this%size = INITIAL_SIZE
        this%count = 0
    end subroutine init

    ! Hash function to map real keys to array indices
    function hash_function(key, size) result(hash_idx)
        real, intent(in) :: key
        integer, intent(in) :: size
        integer :: hash_idx
        real :: scaled_key

        scaled_key = key * 1e6   ! Scale to avoid precision issues
        hash_idx = mod(int(scaled_key), size)  ! Convert to integer and take modulo
    end function hash_function

    ! Insert key-value pair into the hash map
    subroutine insert(this, key, value)
        class(HashMap), intent(inout) :: this
        real, intent(in) :: key
        type(Load), intent(in) :: value
        integer :: idx, original_idx

        ! Get the hash index for the real key
        idx = hash_function(key, this%size)
        original_idx = idx

        ! Linear probing for collision resolution
        do while (this%occupied(idx))
            if (this%keys(idx) == key) then
                stop "Key already exist"
                !print *, "Key already exists!"
                !return
            end if
            idx = mod(idx + 1, this%size)
            if (idx == original_idx) then
                print *, "HashMap is full!"
                return
            end if
        end do

        ! Insert the key-value pair
        this%keys(idx) = key
        this%values(idx) = value
        this%occupied(idx) = .true.
        this%count = this%count + 1

        ! Check load factor and resize if necessary
        if (real(this%count) / real(this%size) > LOAD_FACTOR_THRESHOLD) then
            call this%resize()
        end if
    end subroutine insert

    ! Resize the hash map
    subroutine resize(this)
        class(HashMap), intent(inout) :: this
        real, dimension(:), allocatable :: old_keys
        type(Load), dimension(:), allocatable :: old_values
        logical, dimension(:), allocatable :: old_occupied
        integer :: old_size, i, idx

        old_keys = this%keys
        old_values = this%values
        old_occupied = this%occupied
        old_size = this%size

        ! Double the size of the hash map
        this%size = 2 * old_size
        allocate(this%keys(this%size))
        allocate(this%values(this%size))
        allocate(this%occupied(this%size))

        this%keys = -1.0
        this%occupied = .false.
        this%count = 0

        ! Rehash all existing entries
        do i = 1, old_size
            if (old_occupied(i)) then
                call this%insert(old_keys(i), old_values(i))
            end if
        end do
    end subroutine resize

    ! Search for a key in the hash map
    function search(this, key) result(value)
        class(HashMap), intent(in) :: this
        real, intent(in) :: key
        type(Load) :: value
        integer :: idx, original_idx

        call value%set_start_location(-1.0)
        call value%set_end_location(-1.0)
        call value%set_start_load(-1.0)
        call value%set_end_load(-1.0)

        ! Get the hash index for the real key
        idx = hash_function(key, this%size)
        original_idx = idx

        ! Linear probing to find the key
        do while (this%occupied(idx))
            if (this%keys(idx) == key) then
                value = this%values(idx)
                return
            end if
            idx = mod(idx + 1, this%size)
            if (idx == original_idx) exit
        end do
    end function search

end module load_hashmap
