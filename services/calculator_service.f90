module calculator_service
    use input_manager, only : Input, SupportHashMap, Load, LoadArrayList
    use supports_module, only : Support
    use result_module, only : Result, get_result_rb => get_rb
    implicit none

    type, abstract :: Calculator
        private
        type(Input) :: input_

    contains
        procedure :: init
        procedure(calculating), deferred :: calculate
    end type Calculator

    type, extends(Calculator) :: CalculotrDetermined
    contains
        private
        procedure :: get_anchor_support, get_the_other_support

        procedure, public :: calculate => calculate_determined
    end type CalculotrDetermined

    type, extends(Calculator) :: CalculatorUndetermined
    contains
        procedure :: calculate => calculate_undetermined
    end type CalculatorUndetermined

    abstract interface
        subroutine calculating(this)
            import :: Calculator
            class(Calculator), intent(in) :: this
        end subroutine calculating
    end interface

    contains

    subroutine init(this, input_)
        class(Calculator), intent(inout) :: this
        type(Input), intent(in) :: input_

        this%input_ = input_
    end subroutine init

    !Determined
    subroutine calculate_determined(this)
        class(CalculotrDetermined), intent(in) :: this

    end subroutine calculate_determined

    function get_reaction_of_(this) result(result_)
        class(CalculotrDetermined), intent(in) :: this
        type(LoadArrayList) :: loads
        type(Result) :: result_
        type(Support) :: anchor, the_other
        type(Load) :: current_load
        real :: reaction
        integer :: i

        anchor = this%get_anchor_support()
        the_other = this%get_the_other_support(anchor)
        loads = this%input_%get_loads()

        do i = 1, loads%get_size()
            current_load = loads%get_load(i)
            if(anchor%get_location() > current_load%get_actual_location()) then
                reaction = reaction + (current_load%get_total_load() * (anchor%get_location() - current_load%get_actual_location()))
            else if (anchor%get_location() < current_load%get_actual_location()) then
                reaction = reaction + (current_load%get_total_load() * (current_load%get_actual_location() - anchor%get_location()))
            end if
        end do

        reaction = reaction / (anchor%get_location() - the_other%get_location())

        call result_%set_ra(reaction)
        call result_%set_rb(loads%sum_of_loads() - reaction)

    end function get_reaction_of_

    function get_the_other_support(this, anchor) result(the_other)
        class(CalculotrDetermined), intent(in) :: this
        type(Support), intent(in) :: anchor
        type(Support) ::  the_other, temp
        type(SupportHashMap) :: sup_map
        real, dimension(:), allocatable :: keys
        integer :: i

        sup_map = this%input_%get_supports()
        keys = sup_map%get_keys()

        do i = 1, sup_map%get_size()
            temp = sup_map%get_support(keys(i))
            if(temp%get_location() /= anchor%get_location()) the_other = temp
        end do

    end function get_the_other_support

    function get_anchor_support(this) result (anchor)
        class(CalculotrDetermined), intent(in) :: this
        type(Support) :: anchor, temp
        type(SupportHashMap) :: sup_map
        real, dimension(:), allocatable :: keys
        integer :: i

        call anchor%set_location(-1.0)

        sup_map = this%input_%get_supports()
        keys = sup_map%get_keys()

        do i = 1, sup_map%get_size()
            temp = sup_map%get_support(keys(i))
            if(temp%get_location() > anchor%get_location()) anchor = temp
        end do
    end function get_anchor_support




    !Undetermined
    subroutine calculate_undetermined(this)
        class(CalculatorUndetermined), intent(in) :: this

    end subroutine calculate_undetermined

end module calculator_service
