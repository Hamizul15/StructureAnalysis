# 1 "D:/Programming/Fortran/StructureAnalysis/managers/input_manager.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/managers/input_manager.f90"
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
        procedure :: start !, init
        !getter
        procedure :: get_length, get_number_of_loads, get_number_of_supports, get_supports, get_loads

    end type Input

contains

    !subroutine init(this)
    !    class(Input), intent(inout) :: this
    !
    !   call this%support_map%init_support_map()
    !end subroutine init

    subroutine start(this)
        class(Input), intent(inout) :: this
        class(Support), allocatable :: sup_
        integer :: i

        !call this%init()
        this%length = get_real("- Masukkan panjang beam: ", 0.0)

        this%number_of_loads = get_integer("- Masukkan jumlah tipe beban: ", 0)
        do i = 1, this%number_of_loads
            call this%load_array%add_load(new_load(this%length))
        end do

        this%number_of_supports = get_integer("- Masukkan jumlah tipe support: ", 0)
        do i = 1, this%number_of_supports
            allocate(sup_)
            sup_ = new_support(this%length)
            call this%support_map%insert_support(sup_%get_location(), sup_)
            deallocate(sup_)
        end do

    end subroutine start

    real function get_length(this)
        class(Input), intent(in) :: this

        get_length = this%length
    end function get_length

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
