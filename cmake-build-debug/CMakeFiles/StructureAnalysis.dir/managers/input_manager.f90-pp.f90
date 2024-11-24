# 1 "D:/Programming/Fortran/StructureAnalysis/managers/input_manager.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/managers/input_manager.f90"
! Created by hamiz on 10/21/2024.
module input_manager
    use input_service
    use supports_module
    use loads_module
implicit none

    type :: Input
        private
        real :: length
        integer :: number_of_loads
        integer :: number_of_supports
        type(Load), dimension(:), allocatable :: loads_(:)
        type(Support), dimension(:), allocatable :: supports_(:)

        contains
        !setter
        procedure :: start
        !procedure :: set_length
        !procedure :: set_number_of_loads
        !procedure :: set_number_of_supports
        !getter
        procedure :: get_length, get_number_of_loads, get_number_of_supports

    end type Input

    contains

    subroutine start(this)
        class(Input), intent(inout) :: this

        this%length = get_real("Masukkan panjang beam: ", 0.0)

        this%number_of_loads = get_integer("Masukkan jumlah tipe beban: ", 0)
        allocate(this%loads_(this%number_of_loads))

        this%number_of_supports = get_integer("Masukkan jumlah tipe support: ", 0)
        allocate(this%supports_(this%number_of_supports))

    end subroutine start

    subroutine set_length(this, length_)
        class(Input), intent(inout) :: this
        real, intent(in) :: length_

        this%length = length_
    end subroutine set_length

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

        get_number_of_supports = size(this%supports_)
    end function get_number_of_supports

end module input_manager
