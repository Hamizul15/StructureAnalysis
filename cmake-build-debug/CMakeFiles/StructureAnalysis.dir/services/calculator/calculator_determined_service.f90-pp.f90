# 1 "D:/Programming/Fortran/StructureAnalysis/services/calculator/calculator_determined_service.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/services/calculator/calculator_determined_service.f90"
! Created by hamiz on 11/27/2024.
module calculator_determined_service
    use calculator_service
    use resultload_arraylist

    type, extends(Calculator) :: CalculatorDetermined
    contains
        private
        ! two support
        procedure :: calculate_with_two_support
        procedure :: get_anchor_support
        procedure :: get_the_other_support
        procedure :: get_two_support_result
        ! fixed support
        procedure :: calculate_with_fixed_support
        procedure :: get_fixed_support_result


        procedure, public :: calculate => calculate_determined
    end type CalculatorDetermined

    contains

        subroutine calculate_determined(this)
            class(CalculatorDetermined), intent(inout) :: this

            call this%calculate_with_two_support()
        end subroutine calculate_determined

        !Two Support
        subroutine calculate_with_two_support(this)
            class(CalculatorDetermined), intent(inout) :: this
            type(Result) :: result_
            type(ResultLoad) :: va, vb
            type(LoadArrayList) :: loads
            type(Input) :: input_

            input_ = this%get_input()
            loads = input_%get_loads()
            result_ = this%get_two_support_result()
            va = result_%get_ra()
            vb = result_%get_rb()

            print *, loads%sum_of_loads()
            print *, "Va = ",  va%get_load()
            print *, "Vb = ",  vb%get_load()

        end subroutine calculate_with_two_support

        function get_two_support_result(this) result(result_)
            class(CalculatorDetermined), intent(inout) :: this
            type(LoadArrayList) :: loads
            type(Result) :: result_
            type(Support) :: anchor, the_other
            type(Load) :: current_load
            real :: reaction, distance_to_anchor
            integer :: i
            type(Input) :: input_

            input_ = this%get_input()
            anchor = this%get_anchor_support()
            the_other = this%get_the_other_support(anchor)
            loads = input_%get_loads()

            do i = 1, loads%get_size()
                current_load = loads%get_load(i)
                !print *, ""
                !print *, "Total Load = ", current_load%get_total_load()
                !print *, "Actual Location = ", current_load%get_actual_location()
                !print *, "Anchor Location = ", anchor%get_location()
                !print *, ""

                !if(anchor%get_location() > current_load%get_actual_location()) then
                if(current_load%get_type() /= 2) then
                    distance_to_anchor = anchor%get_location() - current_load%get_actual_location()
                    reaction = reaction + (current_load%get_total_load() * distance_to_anchor)
                end if
                !else if (anchor%get_location() < current_load%get_actual_location()) then
                !reaction = reaction + (current_load%get_total_load() * (current_load%get_actual_location() - anchor%get_location()))
                !end if
            end do

            reaction = reaction / (anchor%get_location() - the_other%get_location())

            call result_%set_ra(new_resultload(the_other%get_location(), reaction))
            call result_%set_rb(new_resultload(anchor%get_location() , loads%sum_of_loads() - reaction))

        end function get_two_support_result

        function get_the_other_support(this, anchor) result(the_other)
            class(CalculatorDetermined), intent(inout) :: this
            type(Support), intent(in) :: anchor
            type(Support) ::  the_other, temp
            type(SupportHashMap) :: sup_map
            real, dimension(:), allocatable :: keys
            integer :: i
            type(Input) :: input_

            input_ = this%get_input()
            sup_map = input_%get_supports()
            keys = sup_map%get_keys()

            do i = 1, sup_map%get_size()
                temp = sup_map%get_support_by_key(keys(i))
                if(temp%get_location() /= anchor%get_location()) the_other = temp
            end do

        end function get_the_other_support

        function get_anchor_support(this) result (anchor)
            class(CalculatorDetermined), intent(inout) :: this
            type(Support) :: anchor, temp
            type(SupportHashMap) :: sup_map
            real, dimension(:), allocatable :: keys
            integer :: i
            type(Input) :: input_

            call anchor%set_location(-1.0)

            input_ = this%get_input()
            sup_map = input_%get_supports()
            keys = sup_map%get_keys()

            do i = 1, sup_map%get_size()
                temp = sup_map%get_support_by_key(keys(i))
                if(temp%get_location() > anchor%get_location()) anchor = temp
            end do
        end function get_anchor_support



        !Fixed Support
        subroutine calculate_with_fixed_support(this)
            class(CalculatorDetermined), intent(inout) :: this
            type(Result) :: result_
            type(Input) :: input_
            type(ResultLoad) :: ma, ra

            input_ = this%get_input()
            result_ = this%get_fixed_support_result()
            ra = result_%get_ra()
            ma = result_%get_ma()

            print *, "Va = ",  ra%get_load()
            print *, "Ma = ", ma%get_load()


        end subroutine calculate_with_fixed_support

        function get_fixed_support_result(this) result(result_)
            class(CalculatorDetermined), intent(inout) :: this
            type(LoadArrayList) :: loads
            type(SupportHashMap) :: sup_map
            type(Support), allocatable :: supports(:)
            type(Result) :: result_
            type(Support) :: anchor
            type(Load) :: current_load
            real :: ma
            integer :: i
            type(Input) :: input_

            input_ = this%get_input()
            sup_map = input_%get_supports()
            supports = sup_map%get_values()
            anchor = supports(1)
            loads = input_%get_loads()

            do i = 1, loads%get_size()
                current_load = loads%get_load(i)
                if(current_load%get_type() /= 2) then
                    ma = ma + (current_load%get_total_load() * (anchor%get_location() - current_load%get_actual_location()) * -1)
                else
                    ma = ma + current_load%get_total_load()
                end if
            end do

            call result_%set_ra(new_resultload(anchor%get_location(), loads%sum_of_loads()))
            call result_%set_ma(new_resultload(anchor%get_location(), ma))

        end function get_fixed_support_result

end module calculator_determined_service
