!using singleton pattern. i dont like it
module location_manager
    use real_set
    use location_interval_module
    use location_interval_arraylist, only : LocationIntervalArrayList
    implicit none

    type :: LocationManager
        private
        type(RealSet) :: locations
        type(LocationIntervalArrayList) :: intervals
    end type LocationManager

    ! Singleton instance of LocationManager
    private
    type(LocationManager), pointer :: location_manager_instance => null()

    public :: construct_location_manager, dispose_location_manager
    public :: set_locations
    public :: get_locations
    public :: get_intervals
    public :: add_location

    contains
        subroutine construct_location_manager()
            if (.not.associated(location_manager_instance)) then
                allocate(location_manager_instance)
            end if
        end subroutine construct_location_manager

        subroutine dispose_location_manager()
            deallocate(location_manager_instance)
        end subroutine  dispose_location_manager

        subroutine set_locations(locations)
            type(RealSet), intent(in) :: locations

            if (associated(location_manager_instance)) then
                location_manager_instance%locations = locations
            end if
        end subroutine set_locations

        function get_locations() result(locations)
            type(RealSet) :: locations

            if (associated(location_manager_instance)) then
                locations = location_manager_instance%locations
            end if
        end function get_locations

        subroutine add_location(location)
            real, intent(in) :: location

            if (associated(location_manager_instance)) then
                call location_manager_instance%locations%add_real_to_set(location)
            end if
        end subroutine add_location

        function get_intervals() result(intervals)
            type(LocationIntervalArrayList) :: intervals
            type(LocationInterval) :: current_inter
            type(RealSet) :: locations
            integer :: i

            if (associated(location_manager_instance)) then
                locations = location_manager_instance%locations
                call locations%sort_real_set()

                do i = 1, (locations%get_size() - 1)
                    call current_inter%set_start(locations%get_real_from_set(i))
                    call current_inter%set_end(locations%get_real_from_set(i + 1))
                    call intervals%add_location_interval(current_inter)
                end do
            end if
        end function get_intervals

end module location_manager
