# 1 "D:/Programming/Fortran/StructureAnalysis/models/supports_module.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/models/supports_module.f90"
! Created by hamiz on 10/22/2024.

module supports_module
    implicit none

    type :: Support
        private
        real :: location

    contains
        procedure :: set_location
        procedure :: get_location
        procedure :: get_number_of_reaction
    end type Support

    type, extends(Support) :: Pin
    contains
        procedure :: get_number_of_reaction => get_pin_number_of_reaction
    end type Pin

    type, extends(Support) :: Fixed
    contains
        procedure :: get_number_of_reaction => get_fixed_number_of_reaction
    end type Fixed

    type, extends(Support) :: Roller
    contains
        procedure :: get_number_of_reaction => get_roller_number_of_reaction
    end type Roller

    interface assignment(=)
        module procedure assign_support
    end interface

contains

    subroutine set_location(this, new_location)
        class(Support), intent(inout) :: this
        real, intent(in) :: new_location

        this%location = new_location
    end subroutine set_location

    function get_location(this) result(loc)
        class(Support), intent(in) :: this
        real :: loc

        loc = this%location
    end function get_location

    function get_number_of_reaction(this) result(reactions)
        class(Support), intent(in) :: this
        integer :: reactions

        reactions = 0
    end function get_number_of_reaction

    integer function get_pin_number_of_reaction(this)
        class(Pin), intent(in) :: this

        get_pin_number_of_reaction = 2
    end function get_pin_number_of_reaction

    integer function get_roller_number_of_reaction(this)
        class(Roller), intent(in) :: this

        get_roller_number_of_reaction = 1
    end function get_roller_number_of_reaction

    integer function get_fixed_number_of_reaction(this)
        class(Fixed), intent(in) :: this

        get_fixed_number_of_reaction = 3
    end function get_fixed_number_of_reaction


    ! Custom assignment operator for the Load type
    subroutine assign_support(lhs, rhs)
        class(Support), intent(out) :: lhs
        class(Support), intent(in) :: rhs
        lhs%location = rhs%location

    end subroutine assign_support

end module supports_module
