! Created by hamiz on 11/27/2024.
module calculator_determined_service
    use calculator_service
    use resultload_arraylist
    use supports_module
    use sheer_calculator

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
            type(Input) :: input_
            type(SupportHashMap) :: sup_map
            type(Support) :: sup

            input_ = this%get_input()
            sup_map = input_%get_supports()
            sup = sup_map%get_support_by_index(1)

            if (sup%get_type() == FIXED_) then
                call this%calculate_with_fixed_support()
            else
                call this%calculate_with_two_support()
            end if
        end subroutine calculate_determined

        !Two Support
        subroutine calculate_with_two_support(this)
            class(CalculatorDetermined), intent(inout) :: this
            type(Result) :: result_
            type(ResultLoad) :: va, vb, current_sheer
            type(ResultLoadArrayList) :: reactions, sheers
            type(LoadArrayList) :: loads
            type(Input) :: input_
            integer :: i

            input_ = this%get_input()
            loads = input_%get_loads()
            result_ = this%get_two_support_result()
            reactions = result_%get_reactions()
            sheers = result_%get_sheers()

            va = reactions%get_resultload(1)
            vb = reactions%get_resultload(2)

            print *, loads%sum_of_loads()
            print *, "Va = ",  va%get_load()
            print *, "Vb = ",  vb%get_load()

            print *, ""
            print *, "Bidang D"
            do i = 1, sheers%get_size()
                current_sheer = sheers%get_resultload(i)
                print *, current_sheer%get_location(), " - ", current_sheer%get_load()
            end do

        end subroutine calculate_with_two_support

        function get_two_support_result(this) result(result_)
            class(CalculatorDetermined), intent(inout) :: this
            type(Result) :: result_
            type(LoadArrayList) :: loads
            type(Support) :: anchor, the_other
            type(Load) :: current_load
            type(SheerCalculator) :: sheer_calc
            type(ResultLoadArrayList) :: reactions
            real :: reaction, distance_to_anchor
            integer :: i
            type(Input) :: input_

            input_ = this%get_input()
            anchor = this%get_anchor_support()
            the_other = this%get_the_other_support(anchor)
            loads = input_%get_loads()

            do i = 1, loads%get_size()
                current_load = loads%get_load(i)
                if(current_load%get_type() /= 2) then
                    distance_to_anchor = anchor%get_location() - current_load%get_actual_location()
                    reaction = reaction + (current_load%get_total_load() * distance_to_anchor)
                end if
            end do

            reaction = reaction / (anchor%get_location() - the_other%get_location())

            call reactions%add_resultload(new_resultload(the_other%get_location(), reaction))
            call reactions%add_resultload(new_resultload(anchor%get_location() , loads%sum_of_loads() - reaction))
            call result_%set_reactions(reactions)

            call sheer_calc%init(input_, result_%get_reactions())
            call result_%set_sheers(sheer_calc%get_sheers())

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
            type(ResultLoadArrayList) :: reactions, mreactions
            type(ResultLoad) :: ma, ra

            input_ = this%get_input()
            result_ = this%get_fixed_support_result()
            reactions = result_%get_reactions()
            mreactions = result_%get_moment_reactions()

            ra = reactions%get_resultload(1)
            ma = mreactions%get_resultload(1)

            print *, "Va = ",  ra%get_load()
            print *, "Ma = ", ma%get_load()
        end subroutine calculate_with_fixed_support

        function get_fixed_support_result(this) result(result_)
            class(CalculatorDetermined), intent(inout) :: this
            type(Result) :: result_
            type(LoadArrayList) :: loads
            type(SupportHashMap) :: sup_map
            type(ResultLoadArrayList) :: reactions, moment_reactions
            type(Support), allocatable :: supports(:)
            type(Support) :: anchor
            type(Load) :: current_load
            real :: ma, distance_to_anchor
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
                    distance_to_anchor = (anchor%get_location() - current_load%get_actual_location()) * -1
                    ma = ma + (current_load%get_total_load() * distance_to_anchor)
                else
                    ma = ma + current_load%get_total_load()
                end if
            end do

            call reactions%add_resultload(new_resultload(anchor%get_location(), loads%sum_of_loads()))
            call moment_reactions%add_resultload(new_resultload(anchor%get_location(), ma))

            call result_%set_reactions(reactions)
            call result_%set_moment_reactions(moment_reactions)
        end function get_fixed_support_result

end module calculator_determined_service