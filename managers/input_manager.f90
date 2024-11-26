! Created by hamiz on 11/26/2024.
module input_manager
    use input_service
    use supports_module
    use loads_module
    use load_arraylist
    use support_hashmap
    implicit none

    type :: Input
        private
        real :: length
        integer :: number_of_loads
        integer :: number_of_supports
        type(SupportHashMap) :: support_map
        type(LoadArrayList) :: load_array

    contains
        procedure :: init, start
        !getter
        procedure :: get_length, get_number_of_loads, get_number_of_supports

    end type Input

contains

    subroutine init(this)
        class(Input), intent(inout) :: this

        !call this%load_map%init_load_map()
        call this%support_map%init_support_map()
    end subroutine init

    subroutine start(this)
        class(Input), intent(inout) :: this
        class(Load), allocatable :: load_
        integer :: i

        this%length = get_real("Masukkan panjang beam: ", 0.0)
        this%number_of_loads = get_integer("Masukkan jumlah tipe beban: ", 0)
        do i = 1, this%number_of_loads
            call this%load_array%add(new_load(this%length))
        end do

        this%number_of_supports = get_integer("Masukkan jumlah tipe support: ", 0)

    end subroutine start

    real function get_length(this)
        class(Input), intent(in) :: this

        get_length = this%length
    end function get_length

    real function get_number_of_loads(this)
        class(Input), intent(in) :: this

        get_number_of_loads = this%number_of_loads
    end function get_number_of_loads

    real function get_number_of_supports(this)
        class(Input), intent(in) :: this

        get_number_of_supports = this%number_of_supports
    end function get_number_of_supports

end module input_manager