module calculator_service
    use input_manager, only : Input
    use supports_module, only : Support
    use result_module, only : Result, ResultLoad
    implicit none

    type, abstract :: Calculator
        private
        type(Input) :: input_

    contains
        procedure :: init, get_input
        procedure(calculate), deferred :: calculate
    end type Calculator

    type, extends(Calculator) :: CalculatorUndetermined
    contains
        procedure :: calculate => calculate_undetermined
    end type CalculatorUndetermined

    abstract interface
        subroutine calculate(this)
            import :: Calculator
            class(Calculator), intent(inout) :: this
        end subroutine calculate
    end interface

    contains

    subroutine init(this, input_)
        class(Calculator), intent(inout) :: this
        type(Input), intent(in) :: input_

        this%input_ = input_
    end subroutine init

    function get_input(this) result(input_)
        class(Calculator), intent(inout) :: this
        type(Input) :: input_

        input_ = this%input_
    end function get_input

    !Undetermined
    subroutine calculate_undetermined(this)
        class(CalculatorUndetermined), intent(inout) :: this

    end subroutine calculate_undetermined

end module calculator_service
