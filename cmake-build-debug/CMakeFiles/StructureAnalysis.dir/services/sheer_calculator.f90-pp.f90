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
        real :: i, step, current_loc, total_load

        step = this%input_%get_length() / this%input_%get_step_for_md()
        do i = 0, this%input_%get_length(), step
            current_loc = i
            total_load = this%get_current_sum_of_loads(current_loc) + this%get_current_sum_of_reactions(current_loc)
            call sheers%add_resultload(new_resultload(current_loc, total_load))
        end do

    end function get_sheers

    function get_current_sum_of_loads(this, current_loc) result(sum)
        class(SheerCalculator), intent(inout) :: this
        real, intent(in) :: current_loc
        real :: sum
        real :: distance, numerator, denominator
        type(Load) :: current_load
        integer :: i

        sum = 0.0
        do i = 1, this%loads%get_size()
            current_load = this%loads%get_load(i)
            if ((current_load%get_start_location() < current_loc).and.(current_load%get_type() /= MOMENT_)) then
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

    function get_current_sum_of_reactions(this, current_loc) result(sum)
        class(SheerCalculator), intent(inout) :: this
        real, intent(in) :: current_loc
        real :: sum
        type(ResultLoad) :: current_reaction
        integer :: i

        sum = 0.0
        do i = 1, this%reactions%get_size()
            current_reaction = this%reactions%get_resultload(i)
            if (current_reaction%get_location() < current_loc) then
                    sum = sum + current_reaction%get_load()
            end if
        end do
    end function get_current_sum_of_reactions

end module sheer_calculator
