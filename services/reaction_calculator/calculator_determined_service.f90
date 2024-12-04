! Created by hamiz on 11/27/2024.
module calculator_determined_service
    use calculator_service
    use resultload_arraylist
    use supports_module
    use sheer_calculator
    use moment_calculator
    use location_manager
    use io_service

    implicit none

    type, extends(Calculator) :: CalculatorDetermined
    contains
        private
        procedure :: print_sheer
        procedure :: print_moment
        procedure :: print_load
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

        subroutine print_load(this, label, loc, load)
            class(CalculatorDetermined), intent(inout) :: this
            character(*) :: label
            real :: loc, load

            write(*, '(A2)', advance='NO') label
            write(*, '(A)', advance='NO') "("
            write(*, '(F6.2)', advance='NO') loc
            write(*, '(A)', advance='NO') ")"
            write(*, '(A4)', advance='NO') "="
            write(*, '(F8.2)') load
        end subroutine print_load

        subroutine print_sheer(this, sheers)
            class(CalculatorDetermined), intent(inout) :: this
            type(ResultLoadArrayList), allocatable :: sheers(:)
            type(ResultLoadArrayList) :: current_sheers
            type(ResultLoad) :: current_sheer
            type(LocationIntervalArrayList) :: intervals
            type(LocationInterval) :: current_inter
            integer :: i, j

            intervals = get_intervals()

            print *, ""
            print *, "==========================================="
            print *, "Bidang D"
            print *, "==========================================="
            do i = 1, intervals%get_size()
                current_inter = intervals%get_location_lnterval(i)
                current_sheers = sheers(i)

                print *, ""
                write(*, '(A8)', advance='NO') "Interval"
                write(*, '(F8.2)', advance='NO') current_inter%get_start()
                write(*, '(A16)', advance='NO') " <= x <= "
                write(*, '(F16.2)', advance='NO') current_inter%get_end()
                print *, ""
                write(*, '(A16)', advance='NO') "Lokasi"
                write(*, '(A16)', advance='NO') " --> "
                write(*, '(A16)') "Beban"
                do j = 1, current_sheers%get_size()
                    current_sheer = current_sheers%get_resultload(j)
                    write(*, '(F16.2)', ADVANCE='NO') current_sheer%get_location()
                    write(*, '(A16)', ADVANCE='NO') " --> "
                    write(*, '(F16.2)') current_sheer%get_load()
                end do
            end do
            print *, ""
        end subroutine print_sheer

        subroutine print_moment(this, moments)
            class(CalculatorDetermined), intent(inout) :: this
            type(ResultLoadArrayList), allocatable :: moments(:)
            type(ResultLoadArrayList) :: current_moments
            type(ResultLoad) :: current_moment
            type(LocationIntervalArrayList) :: intervals
            type(LocationInterval) :: current_inter
            integer :: i, j

            intervals = get_intervals()

            print *, ""
            print *, "==========================================="
            print *, "Bidang M"
            print *, "==========================================="
            do i = 1, intervals%get_size()
                current_inter = intervals%get_location_lnterval(i)
                current_moments = moments(i)

                print *, ""
                write(*, '(A8)', advance='NO') "Interval"
                write(*, '(F8.2)', advance='NO') current_inter%get_start()
                write(*, '(A16)', advance='NO') " <= x <= "
                write(*, '(F16.2)', advance='NO') current_inter%get_end()
                print *, ""
                write(*, '(A16)', advance='NO') "Lokasi"
                write(*, '(A16)', advance='NO') " --> "
                write(*, '(A16)') "Momen"
                do j = 1, current_moments%get_size()
                    current_moment = current_moments%get_resultload(j)
                    write(*, '(F16.2)', ADVANCE='NO') current_moment%get_location()
                    write(*, '(A16)', ADVANCE='NO') " --> "
                    write(*, '(F16.2)') current_moment%get_load()
                end do
            end do
            print *, ""
        end subroutine print_moment

        !Two Support
        subroutine calculate_with_two_support(this)
            class(CalculatorDetermined), intent(inout) :: this
            type(Result) :: result_
            type(ResultLoad) :: va, vb, current_sheer
            type(ResultLoadArrayList) :: reactions
            type(LoadArrayList) :: loads
            type(Input) :: input_
            integer :: i

            input_ = this%get_input()
            loads = input_%get_loads()
            result_ = this%get_two_support_result()
            reactions = result_%get_reactions()

            va = reactions%get_resultload(1)
            vb = reactions%get_resultload(2)

            print *, ""
            print *, "==========================================="
            call this%print_load("Va", va%get_location(), va%get_load())
            call this%print_load("Vb", vb%get_location(), vb%get_load())
            print *, "==========================================="
            call this%print_sheer(result_%get_sheers())
            call this%print_moment(result_%get_moments())

            call write_output(result_)
        end subroutine calculate_with_two_support

        function get_two_support_result(this) result(result_)
            class(CalculatorDetermined), intent(inout) :: this
            type(Result) :: result_
            type(LoadArrayList) :: loads
            type(Support) :: anchor, the_other
            type(Load) :: current_load
            type(SheerCalculator) :: sheer_calc
            type(MomentCalculator) :: moment_calc
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
                if(current_load%get_type() /= MOMENT_) then
                    distance_to_anchor = anchor%get_location() - current_load%get_actual_location()
                    reaction = reaction + (current_load%get_total_load() * distance_to_anchor)
                else
                    reaction = reaction + current_load%get_total_load() * -1
                end if
            end do

            reaction = reaction / (anchor%get_location() - the_other%get_location())

            call reactions%add_resultload(new_resultload(the_other%get_location(), reaction))
            call reactions%add_resultload(new_resultload(anchor%get_location() , loads%sum_of_loads() - reaction))
            call result_%set_reactions(reactions)

            call sheer_calc%init(input_, reactions)
            call result_%set_sheers(sheer_calc%get_sheers())

            call moment_calc%init(input_, reactions)
            call result_%set_moments(moment_calc%get_moments())

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
            type(ResultLoadArrayList) :: reactions, mreactions
            type(ResultLoad) :: ma, ra

            result_ = this%get_fixed_support_result()
            reactions = result_%get_reactions()
            mreactions = result_%get_moment_reactions()

            ra = reactions%get_resultload(1)
            ma = mreactions%get_resultload(1)

            print *, ""
            print *, "==========================================="
            call this%print_load("Va", ra%get_location(), ra%get_load())
            call this%print_load("Ma", ma%get_location(), ma%get_load())
            print *, "==========================================="
            call this%print_sheer(result_%get_sheers())
            call this%print_moment(result_%get_moments())

            call write_output(result_)
        end subroutine calculate_with_fixed_support

        function get_fixed_support_result(this) result(result_)
            class(CalculatorDetermined), intent(inout) :: this
            type(Result) :: result_
            type(LoadArrayList) :: loads
            type(SupportHashMap) :: sup_map
            type(ResultLoadArrayList) :: reactions, moment_reactions
            type(Support), allocatable :: supports(:)
            type(Support) :: anchor
            type(SheerCalculator) :: sheer_calc
            type(MomentCalculator) :: moment_calc
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
                if(current_load%get_type() /= MOMENT_) then
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

            call sheer_calc%init(input_, reactions)
            call result_%set_sheers(sheer_calc%get_sheers())

            call moment_calc%init(input_, reactions, moment_reactions)
            call result_%set_moments(moment_calc%get_moments())

        end function get_fixed_support_result

end module calculator_determined_service