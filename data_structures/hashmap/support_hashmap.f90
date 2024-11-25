module support_hashmap
    use supports_module
    implicit none

    integer, parameter :: INITIAL_SIZE = 100
    real, parameter :: LOAD_FACTOR_THRESHOLD = 0.75

    type :: SupportHashMap
        private
        real, dimension(:), allocatable :: keys
        class(Support), dimension(:), allocatable :: values(:)
        logical, dimension(:), allocatable :: occupied
        integer :: size
        integer :: count
    contains
        procedure :: init, insert, search, resize, get_count, get_values
    end type SupportHashMap

contains

    function get_count(this) result(co)
        class(SupportHashMap), intent(in) :: this
        integer :: co

        co = this%count
    end function get_count

    function get_values(this) result(valuess)
        class(SupportHashMap), intent(in) :: this
        class(Support), dimension(:), allocatable :: valuess(:)

        valuess = this%values
    end function get_values

    ! Initialize the hash map
    subroutine init(this)
        class(SupportHashMap), intent(out) :: this
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
            call this%values(i)%set_location(-1.0)
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
        class(SupportHashMap), intent(inout) :: this
        real, intent(in) :: key
        class(Support), allocatable, intent(in) :: value
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
        class(SupportHashMap), intent(inout) :: this
        real, dimension(:), allocatable :: old_keys
        class(Support), dimension(:), allocatable :: old_values(:)
        logical, dimension(:), allocatable :: old_occupied
        integer :: old_size, i, idx
        class(Support), allocatable :: temp_value

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
                ! Allocate a temporary value to pass to insert
                allocate(temp_value)
                temp_value = old_values(i)
                call this%insert(old_keys(i), temp_value)
            end if
        end do
    end subroutine resize

    ! Search for a key in the hash map
    function search(this, key) result(value)
        class(SupportHashMap), intent(in) :: this
        real, intent(in) :: key
        class(Support), allocatable :: value
        integer :: idx, original_idx

        idx = hash_function(key, this%size)
        original_idx = idx

        do while (this%occupied(idx))
            if (this%keys(idx) == key) then
                if (.not.allocated(value)) then
                    select type(stored => this%values(idx))
                    type is (Pin)
                        allocate(Pin :: value)  ! Correctly allocate for Pin type
                    type is (Roller)
                        allocate(Roller :: value)  ! Correctly allocate for Roller type
                    type is (Fixed)
                        allocate(Fixed :: value)  ! Correctly allocate for Fixed type
                    class default
                        allocate(Support :: value)  ! Fallback to general Support
                    end select
                end if
                value = this%values(idx)  ! Assign the value to the polymorphic variable
                return
            end if
            idx = mod(idx + 1, this%size)
            if (idx == original_idx) exit
        end do
    end function search

end module support_hashmap