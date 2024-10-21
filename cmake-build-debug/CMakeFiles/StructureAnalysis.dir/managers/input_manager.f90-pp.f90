# 1 "D:/Programming/Fortran/StructureAnalysis/managers/input_manager.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/managers/input_manager.f90"
! Created by hamiz on 10/21/2024.
module input_manager
implicit none

    type :: Input
        private
        real :: length

        contains
        procedure :: set_length
        procedure :: get_length

    end type Input

    contains

    subroutine set_length(this, length_)
        class(Input), intent(inout) :: this
        real, intent(in) :: length_

        this%length = length_
    end subroutine set_length

    real function get_length(this)
        class(Input), intent(in) :: this

        get_length = this%length
    end function get_length

end module input_manager
