# 1 "D:/Programming/Fortran/StructureAnalysis/services/moment_calculator.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/services/moment_calculator.f90"
! Created by hamiz on 11/29/2024.
module moment_calculator
    use loads_module
    use input_manager, only : Input, LoadArrayList, Load
    use result_module, only : get_data_sheers => get_sheers
    use resultload_arraylist, only : ResultLoadArrayList, ResultLoad, new_resultload
    use location_manager
    use location_interval_arraylist, only : LocationInterval, LocationIntervalArrayList
    implicit none

    type MomentCalculator
        private
        type(Input) :: input_
        type(ResultLoadArrayList) :: reactions
        type(ResultLoadArrayList) :: moments
        type(LoadArrayList) :: loads
        type(LoadArrayList) :: moment_loads
        type(LoadArrayList) :: non_moment_loads

        contains
            private
            procedure :: get_current_moment_of_loads
            procedure :: get_current_moment_of_reactions
            procedure :: get_current_sum_of_moments
            procedure :: find_current_load_of_distributed
            procedure :: find_current_center_location_of_distributed
            procedure :: get_proper_condition

            procedure, public :: init, get_moments
    end type MomentCalculator

    contains

        subroutine init(this, input_, reactions, mreactions)
            class(MomentCalculator), intent(inout) :: this
            type(Input) :: input_
            type(ResultLoadArrayList) :: reactions
            type(ResultLoadArrayList), optional :: mreactions

            this%input_ = input_
            this%loads = input_%get_loads()
            this%non_moment_loads = this%loads %get_non_moments()
            this%moment_loads = this%loads%get_moments()
            this%reactions = reactions

            if(present(mreactions)) this%moments = mreactions

        end subroutine init

        !function get_moments(this) result(moments)
        !    class(MomentCalculator), intent(inout) :: this
        !    type(ResultLoadArrayList) :: moments
        !    type(LocationIntervalArrayList) :: intervals
        !    type(LocationInterval) :: current_inter
        !    real :: current_loc, total_load, moment_of_reactions, moments_of_loads, sum_of_moments
        !    integer :: i, iteration = 1

        !    intervals = get_intervals()
        !    print *, ""
        !    print *, "Bidang M"
        !    do i = 1, intervals%get_size()
        !        current_inter = intervals%get_location_lnterval(i)
        !        print *, ""
        !        print *, current_inter%get_start(), " <= x <= ", current_inter%get_end()
        !        do current_loc = current_inter%get_start(), current_inter%get_end()
        !            moments_of_loads = this%get_current_moment_of_loads(current_loc, iteration)
        !            moment_of_reactions = this%get_current_moment_of_reactions(current_loc, iteration)
        !            sum_of_moments = this%get_current_sum_of_moments(current_loc, iteration)
        !            total_load = moments_of_loads + moment_of_reactions + sum_of_moments
        !            call moments%add_resultload(new_resultload(current_loc, total_load))

        !            write(*, '(F8.2)', ADVANCE='NO') current_loc
        !            write(*, '(A)', ADVANCE='NO') " --> "
        !            write(*, '(F8.2)', ADVANCE='NO') moments_of_loads
        !            write(*, '(A)', ADVANCE='NO') " + "
        !            write(*, '(F8.2)', ADVANCE='NO') moment_of_reactions
        !            write(*, '(A)', ADVANCE='NO') " + "
        !            write(*, '(F8.2)', ADVANCE='NO') sum_of_moments
        !            write(*, '(A)', ADVANCE='NO') " = "
        !            write(*, '(F8.2)') total_load
!
        !            iteration = iteration + 1
        !        end do
        !        iteration = 1
        !    end do
        !    print *, ""
        !end function get_moments

        function get_moments(this) result(moments)
            class(MomentCalculator), intent(inout) :: this
            type(ResultLoadArrayList), allocatable :: moments(:)
            type(ResultLoadArrayList) :: current_moments
            type(LocationIntervalArrayList) :: intervals
            type(LocationInterval) :: current_inter
            real :: current_loc, total_load, moment_of_reactions, moments_of_loads, sum_of_moments
            integer :: i, iteration = 1

            intervals = get_intervals()
            allocate(moments(intervals%get_size()))
            do i = 1, intervals%get_size()
                current_inter = intervals%get_location_lnterval(i)
                do current_loc = current_inter%get_start(), current_inter%get_end()
                    moments_of_loads = this%get_current_moment_of_loads(current_loc, iteration)
                    moment_of_reactions = this%get_current_moment_of_reactions(current_loc, iteration)
                    sum_of_moments = this%get_current_sum_of_moments(current_loc, iteration)
                    total_load = moments_of_loads + moment_of_reactions + sum_of_moments
                    call current_moments%add_resultload(new_resultload(current_loc, total_load))
                    iteration = iteration + 1
                end do
                moments(i) = current_moments
                call current_moments%clear_resultloads()
                iteration = 1
            end do
        end function get_moments

        function find_current_load_of_distributed(this, current_loc, distributed_load) result(cld)
            class(MomentCalculator), intent(inout) :: this
            real, intent(in) :: current_loc
            type(Load), intent(in) :: distributed_load
            real :: cld, length, numerator, denominator, the_height

            length = current_loc - distributed_load%get_start_location()
            numerator = (distributed_load%get_end_load() - distributed_load%get_start_load()) * -1
            denominator = distributed_load%get_end_location() - distributed_load%get_start_location()
            the_height = (((numerator / denominator) * length) + (distributed_load%get_start_load() * -1))

            cld = (distributed_load%get_start_load() * -1 + the_height) * length / 2
        end function find_current_load_of_distributed

        function find_current_center_location_of_distributed(this, current_loc, distributed_load) result(loc)
            class(MomentCalculator), intent(inout) :: this
            real, intent(in) :: current_loc
            type(Load), intent(in) :: distributed_load
            real :: loc, length, first_part, second_part

            length = current_loc - distributed_load%get_start_location()
            first_part = (distributed_load%get_start_load() + 2 * distributed_load%get_end_load()) * length
            second_part = 3 * (distributed_load%get_start_load() + distributed_load%get_end_load())
            loc = distributed_load%get_start_location() + (first_part / second_part)
        end function find_current_center_location_of_distributed

        function get_current_moment_of_loads(this, current_loc, iteration) result(sum)
            class(MomentCalculator), intent(inout) :: this
            real, intent(in) :: current_loc
            real :: sum, distance
            type(Load) :: current_load
            integer :: i, iteration

            sum = 0.0
            do i = 1, this%non_moment_loads%get_size()
                current_load = this%non_moment_loads%get_load(i)
                if (this%get_proper_condition(iteration, current_loc, current_load%get_start_location())) then
                    !check wheteher location is between distributed load
                    if((current_load%get_type() == DISTRIBUTED_).and.(current_loc < current_load%get_end_location())) then
                        distance = current_loc - this%find_current_center_location_of_distributed(current_loc, current_load)
                        sum = sum + this%find_current_load_of_distributed(current_loc, current_load) * distance
                    else
                        distance = current_loc - current_load%get_actual_location()
                        sum = sum + current_load%get_total_load() * distance * -1
                    end if
                end if
            end do
        end function get_current_moment_of_loads

        function get_current_moment_of_reactions(this, current_loc, iteration) result(sum)
            class(MomentCalculator), intent(inout) :: this
            real, intent(in) :: current_loc
            real :: sum, distance
            type(ResultLoad) :: current_reaction
            integer :: i, iteration

            sum = 0.0
            do i = 1, this%reactions%get_size()
                current_reaction = this%reactions%get_resultload(i)
                if(current_reaction%get_location() == this%input_%get_length()) cycle

                distance = current_loc - current_reaction%get_location()
                if (this%get_proper_condition(iteration, current_loc, current_reaction%get_location())) then
                    sum = sum + current_reaction%get_load() * distance
                end if
            end do
        end function get_current_moment_of_reactions

        function get_current_sum_of_moments(this, current_loc, iteration) result(sum)
            class(MomentCalculator), intent(inout) :: this
            real, intent(in) :: current_loc
            real :: sum
            type(Load) :: current_moment
            type(ResultLoad) :: current_mreaction
            integer :: i, iteration
            logical :: the_condition

            sum = 0.0
            do i = 1, this%moment_loads%get_size()
                current_moment = this%moment_loads%get_load(i)
                if (this%get_proper_condition(iteration, current_loc, current_moment%get_start_location())) then
                    sum = sum + current_moment%get_total_load()
                end if
            end do

            do i = 1, this%moments%get_size()
                current_mreaction = this%moments%get_resultload(i)
                if (this%get_proper_condition(iteration, current_loc, current_mreaction%get_location())) then
                    sum = sum + current_mreaction%get_load() * -1
                end if
            end do
        end function get_current_sum_of_moments

        function get_proper_condition(this, iteration, current_loc, load_location) result(the_condition)
            class(MomentCalculator), intent(inout) :: this
            real, intent(in) :: current_loc, load_location
            logical :: the_condition
            integer :: iteration

            if(iteration == 1) then
                the_condition = load_location <= current_loc
            else
                the_condition = load_location < current_loc
            end if
        end function get_proper_condition

end module moment_calculator
