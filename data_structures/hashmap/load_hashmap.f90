module load_hashmap
    use loads_module
    implicit none

    integer, parameter :: INITIAL_SIZE = 100
    real, parameter :: LOAD_FACTOR_THRESHOLD = 0.75

    type :: LoadHashMap
        private
        real, dimension(:), allocatable :: keys
        class(Load), dimension(:), allocatable :: values(:)
        logical, dimension(:), allocatable :: occupied
        integer :: size
        integer :: count
    contains
        procedure :: init, insert, search, resize
    end type LoadHashMap

contains

    ! Initialize the hash map
    subroutine init(this)
        class(LoadHashMap), intent(out) :: this
        integer :: i

        this%size = INITIAL_SIZE
        this%count = 0

        allocate(this%keys(this%size))
        allocate(this%values(this%size))
        allocate(this%occupied(this%size))

        this%keys = -1.0
        this%occupied = .false.

        do i = 1, this%size
            !allocate(this%values(i))
            call this%values(i)%set_start_location(-1.0)
            call this%values(i)%set_end_location(-1.0)
            call this%values(i)%set_start_load(-1.0)
            call this%values(i)%set_end_load(-1.0)
        end do
    end subroutine init

    ! Hash function to map real keys to array indices
    function hash_function(key, size) result(hash_idx)
        real, intent(in) :: key
        integer, intent(in) :: size
        integer :: hash_idx
        real :: scaled_key

        scaled_key = key * 1e6
        hash_idx = mod(int(scaled_key), size)
    end function hash_function

    ! Insert key-value pair into the hash map
    subroutine insert(this, key, value)
        class(LoadHashMap), intent(inout) :: this
        real, intent(in) :: key
        class(Load), intent(in) :: value
        integer :: idx, original_idx

        idx = hash_function(key, this%size)
        original_idx = idx

        do while (this%occupied(idx))
            if (this%keys(idx) == key) then
                stop "Key already exists!"
            end if
            idx = mod(idx + 1, this%size)
            if (idx == original_idx) then
                print *, "HashMap is full!"
                return
            end if
        end do

        !allocate(this%values(idx))  ! Allocate individual element
        this%keys(idx) = key
        this%values(idx) = value
        this%occupied(idx) = .true.
        this%count = this%count + 1

        if (real(this%count) / real(this%size) > LOAD_FACTOR_THRESHOLD) then
            call this%resize()
        end if
    end subroutine insert

    ! Resize the hash map
    subroutine resize(this)
        class(LoadHashMap), intent(inout) :: this
        real, dimension(:), allocatable :: old_keys
        class(Load), dimension(:), allocatable :: old_values(:)
        logical, dimension(:), allocatable :: old_occupied
        integer :: old_size, i, idx

        old_keys = this%keys
        old_values = this%values
        old_occupied = this%occupied
        old_size = this%size

        this%size = 2 * old_size
        allocate(this%keys(this%size))
        allocate(this%values(this%size))
        allocate(this%occupied(this%size))

        this%keys = -1.0
        this%occupied = .false.
        this%count = 0

        do i = 1, old_size
            if (old_occupied(i)) then
                call this%insert(old_keys(i), old_values(i))
            end if
        end do
    end subroutine resize

    ! Search for a key in the hash map
    function search(this, key) result(value)
        class(LoadHashMap), intent(in) :: this
        real, intent(in) :: key
        class(Load), allocatable :: value
        integer :: idx, original_idx

        idx = hash_function(key, this%size)
        original_idx = idx

        do while (this%occupied(idx))
            if (this%keys(idx) == key) then
                allocate(value)
                value = this%values(idx)
                return
            end if
            idx = mod(idx + 1, this%size)
            if (idx == original_idx) exit
        end do
    end function search

end module load_hashmap
