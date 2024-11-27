# 1 "D:/Programming/Fortran/StructureAnalysis/models/supports_module.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/models/supports_module.f90"
! Created by hamiz on 10/22/2024.
module supports_module
    use input_service
    implicit none

    integer, parameter :: NUMBER_OF_SUPPORTS = 3

    private :: get_types, get_type

    type :: Support
        private
        real :: location
        integer :: type

    contains
        procedure :: set_location
        procedure :: get_location
        procedure :: get_type
        procedure :: get_number_of_reaction
    end type Support

    type, extends(Support) :: Pin
    contains
        !procedure :: get_number_of_reaction => get_pin_number_of_reaction
    end type Pin

    type, extends(Support) :: Fixed
    contains
        !procedure :: get_number_of_reaction => get_fixed_number_of_reaction
    end type Fixed

    type, extends(Support) :: Roller
    contains
        !procedure :: get_number_of_reaction => get_roller_number_of_reaction
    end type Roller

    interface assignment(=)
        module procedure assign_support
    end interface

contains

    function new_support(max) result (sup)
        implicit none
        class(Support), allocatable :: sup
        real, dimension(2) :: allowed_fixed_loc
        real, optional ::  max
        integer :: type_

        allowed_fixed_loc(1) = 0.0
        allowed_fixed_loc(2) = max

        type_ = get_choosen_type()
        if(type_ == 1) allocate(Pin :: sup)
        if(type_ == 2) allocate(Roller :: sup)
        if(type_ == 3) allocate(Fixed :: sup)
        sup%type = type_

        if (type_ == 3) then
            call sup%set_location(get_real_restricted("Masukkan posisi tumpuhan: ", allowed_fixed_loc))
        else
            call sup%set_location(get_real("Masukkan posisi tumpuhan: ", 0.0, max))
        end if
    end function new_support

    function get_choosen_type() result(type_)
        integer :: type_, i
        CHARACTER(len=10), DIMENSION(NUMBER_OF_SUPPORTS) :: types

        types = get_types()

        WRITE (*, '(A)') 'Silahkan pilih tipe tumpuhan'
        DO i = 1, SIZE(types)
            PRINT '(I1.0, A, A)', i, '. ', types(i)
        END DO

        type_ = get_integer("Masukkan tipe tumpuhan: ", 1, size(types))
    end function get_choosen_type

    function get_types() result(types)
        CHARACTER(len=10), DIMENSION(NUMBER_OF_SUPPORTS) :: types
        types(1) = 'Pin'
        types(2) = 'Roller'
        types(3) = 'Fixed'
    end function get_types

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
        if(this%get_type() == 1) reactions = 2
        if(this%get_type() == 2) reactions = 1
        if(this%get_type() == 3) reactions = 3
    end function get_number_of_reaction

    function get_type(this) result(tipe)
        class(Support), intent(in) :: this
        integer :: tipe

        tipe = this%type
    end function get_type

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
        lhs%type = rhs%type

    end subroutine assign_support

end module supports_module
