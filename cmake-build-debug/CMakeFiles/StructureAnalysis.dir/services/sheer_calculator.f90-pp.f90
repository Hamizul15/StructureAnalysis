# 1 "D:/Programming/Fortran/StructureAnalysis/services/sheer_calculator.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/services/sheer_calculator.f90"
! Created by hamiz on 11/28/2024.
module sheer_calculator
    use loads_module
    use input_manager, only : Input, LoadArrayList, Load
    use result_module, only : get_data_sheers => get_sheers
    use resultload_arraylist, only : ResultLoadArrayList, ResultLoad, new_resultload
    use location_manager
    use location_interval_arraylist, only : LocationInterval, LocationIntervalArrayList

    type :: SheerCalculator
        private
        type(Input) :: input_
        type(ResultLoadArrayList) :: reactions
        type(LoadArrayList) :: loads

        contains
        private
        procedure :: get_current_sum_of_loads, get_current_sum_of_reactions, find_current_load_of_distributed

        procedure, public :: init, get_sheers
    end type SheerCalculator

    contains

    subroutine init(this, input_, reactions)
        class(SheerCalculator), intent(inout) :: this
        type(Input) :: input_
        type(ResultLoadArrayList) :: reactions

        this%input_ = input_
        this%loads = input_%get_loads()
        this%reactions = reactions
    end subroutine init

    function get_sheers(this) result(sheers)
        class(SheerCalculator), intent(inout) :: this
        type(ResultLoadArrayList) :: sheers
        type(LocationIntervalArrayList) :: intervals
        type(LocationInterval) :: current_inter
        real :: current_loc, step, total_load, x, y
        integer :: i, iteration = 1

        intervals = get_intervals()
        print *, ""
        print *, "Bidang D"
        do i = 1, intervals%get_size()
            current_inter = intervals%get_location_lnterval(i)
            !step = (current_inter%get_end() - current_inter%get_start()) / 10
            print *, ""
            print *, current_inter%get_start(), " <= x <= ", current_inter%get_end()
            do current_loc = current_inter%get_start(), current_inter%get_end()
                x = this%get_current_sum_of_loads(current_loc, iteration)
                y = this%get_current_sum_of_reactions(current_loc, iteration)
                total_load = x + y
                call sheers%add_resultload(new_resultload(current_loc, total_load))
                print *, current_loc, " --> ", x, " + " , y, " = ", total_load
                iteration = iteration + 1
            end do
            iteration = 1
        end do
        print *, ""
        !step = this%input_%get_length() / 10
        !do i = 1, this%input_%get_length(), step
        !    current_loc = i
        !    total_load = this%get_current_sum_of_loads(current_loc) + this%get_current_sum_of_reactions(current_loc)
        !    call sheers%add_resultload(new_resultload(current_loc, total_load))
        !end do

    end function get_sheers

    function get_current_sum_of_loads(this, current_loc, iteration) result(sum)
        class(SheerCalculator), intent(inout) :: this
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
        class(SheerCalculator), intent(inout) :: this
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
        class(SheerCalculator), intent(inout) :: this
        real, intent(in) :: current_loc
        real :: sum
        type(ResultLoad) :: current_reaction
        integer :: i
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

end module sheer_calculator
