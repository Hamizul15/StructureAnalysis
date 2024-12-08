! Created by hamiz on 11/26/2024.
module input_manager
    use input_service
    use supports_module
    use loads_module
    use load_arraylist
    use support_hashmap
    use io_service
    implicit none

    type :: Input
        private
        real :: length
        integer :: number_of_loads
        integer :: number_of_supports
        integer :: step_for_md
        type(SupportHashMap) :: support_map
        type(LoadArrayList) :: load_array

    contains
        procedure :: start, dispose !, init
        !getter
        procedure :: get_length
        !procedure :: get_step_for_md
        procedure :: get_number_of_loads
        procedure :: get_number_of_supports
        procedure :: get_supports
        procedure :: get_loads

    end type Input

contains

    !subroutine init(this)
    !    class(Input), intent(inout) :: this
    !
    !   call this%support_map%init_support_map()
    !end subroutine init

    subroutine start(this)
        class(Input), intent(inout) :: this
        integer :: i

        !call this%init()
        call add_location(0.0)
        this%length = get_real("- Masukkan panjang beam: ", 0.0)
        call add_location(this%length)

        print *, ""
        this%number_of_loads = get_integer("- Masukkan jumlah beban: ", 0)
        do i = 1, this%number_of_loads
            call this%load_array%add_load(new_load(this%length))
        end do

        print *, ""
        this%number_of_supports = get_integer("- Masukkan jumlah support: ", 0)
        do i = 1, this%number_of_supports
            call this%support_map%insert_support_no_key(new_support(this%length))
        end do

        call write_input(this%length, this%load_array, this%support_map)
    end subroutine start

    subroutine dispose(this)
        class(Input), intent(inout) :: this

        this%length = -1.0
        this%number_of_supports = -1
        this%number_of_loads = -1
        !this%step_for_md = -1
        call this%load_array%clear_loads()
        call this%support_map%clear_supports()
    end subroutine dispose

    real function get_length(this)
        class(Input), intent(in) :: this

        get_length = this%length
    end function get_length

    !real function get_step_for_md(this)
    !    class(Input), intent(in) :: this

    !    get_step_for_md = this%step_for_md
    !end function get_step_for_md

    real function get_number_of_loads(this)
        class(Input), intent(in) :: this

        get_number_of_loads = this%number_of_loads
    end function get_number_of_loads

    function get_number_of_supports(this) result(num_sups)
        class(Input), intent(in) :: this
        real :: num_sups

        num_sups = this%number_of_supports
    end function get_number_of_supports

    function get_loads(this) result(los)
        class(Input), intent(in) :: this
        type(LoadArrayList) :: los

        los = this%load_array
    end function get_loads

    function get_supports(this) result(sups)
        class(Input), intent(in) :: this
        type(SupportHashMap) :: sups

        sups = this%support_map
    end function get_supports


end module input_manager