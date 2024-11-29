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

        contains
            private
            procedure :: get_current_sum_of_loads
            procedure :: get_current_sum_of_reactions
            procedure :: get_current_sum_of_moments
            procedure :: find_current_load_of_distributed

            procedure, public :: init, get_moments
    end type MomentCalculator

    contains

        subroutine init(this, input_, reactions, moments)
            class(MomentCalculator), intent(inout) :: this
            type(Input) :: input_
            type(ResultLoadArrayList) :: reactions, moments

            this%input_ = input_
            this%loads = input_%get_loads()
            this%reactions = reactions
            this%moments = moments
        end subroutine init

        function get_moments(this) result(moments)
            class(MomentCalculator), intent(inout) :: this
            type(ResultLoadArrayList) :: moments
            type(LocationIntervalArrayList) :: intervals
            type(LocationInterval) :: current_inter
            real :: current_loc, total_load, sum_of_reactions, sum_of_loads, sum_of_moments
            integer :: i, iteration = 1

            intervals = get_intervals()
            do i = 1, intervals%get_size()
                current_inter = intervals%get_location_lnterval(i)
                do current_loc = current_inter%get_start(), current_inter%get_end()
                    sum_of_loads = this%get_current_sum_of_loads(current_loc, iteration)
                    sum_of_reactions = this%get_current_sum_of_reactions(current_loc, iteration)
                    sum_of_moments = this%get_current_sum_of_moments(current_loc, iteration)
                    total_load = sum_of_loads + sum_of_reactions

                    call moments%add_resultload(new_resultload(current_loc, total_load))
                    iteration = iteration + 1
                end do
                iteration = 1
            end do
        end function get_moments

        function get_current_sum_of_loads(this, current_loc, iteration) result(sum)
            class(MomentCalculator), intent(inout) :: this
            real, intent(in) :: current_loc
            real :: sum
            real :: distance, numerator, denominator
            type(Load) :: current_load
            integer :: i, iteration
            logical :: the_condition

            sum = 0.0
            do i = 1, this%loads%get_size()
                current_load = this%loads%get_load(i)
                if(current_load%get_type() == MOMENT_) cycle

                if(iteration == 1) then
                    the_condition = current_load%get_start_location() <= current_loc
                else
                    the_condition = current_load%get_start_location() < current_loc
                end if

                if (the_condition) then
                    !check wheteher location is between distributed load
                    if((current_load%get_type() == DISTRIBUTED_).and.(current_loc < current_load%get_end_location())) then
                        sum = sum + this%find_current_load_of_distributed(current_loc, current_load)
                    else
                        sum = sum + current_load%get_total_load() * -1
                    end if
                end if
            end do
        end function get_current_sum_of_loads

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

        function get_current_sum_of_reactions(this, current_loc, iteration) result(sum)
            class(MomentCalculator), intent(inout) :: this
            real, intent(in) :: current_loc
            real :: sum
            type(ResultLoad) :: current_reaction
            integer :: i, iteration
            logical :: the_condition

            sum = 0.0
            do i = 1, this%reactions%get_size()
                current_reaction = this%reactions%get_resultload(i)
                if(current_reaction%get_location() == this%input_%get_length()) cycle

                if(iteration == 1) then
                    the_condition = current_reaction%get_location() <= current_loc
                else
                    the_condition = current_reaction%get_location() < current_loc
                end if

                if (the_condition) then
                    sum = sum + current_reaction%get_load()
                end if
            end do
        end function get_current_sum_of_reactions

        function get_current_sum_of_moments(this, current_loc, iteration) result(sum)
            class(MomentCalculator), intent(inout) :: this
            real, intent(in) :: current_loc
            real :: sum
            type(ResultLoad) :: current_moment
            integer :: i, iteration
            logical :: the_condition

            sum = 0.0
            do i = 1, this%reactions%get_size()
                current_moment = this%reactions%get_resultload(i)

                if(iteration == 1) then
                    the_condition = current_moment%get_location() <= current_loc
                else
                    the_condition = current_moment%get_location() < current_loc
                end if

                if (the_condition) then
                    sum = sum + current_moment%get_load()
                end if
            end do
        end function get_current_sum_of_moments

end module moment_calculator