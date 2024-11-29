# 1 "D:/Programming/Fortran/StructureAnalysis/managers/location_manager.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/managers/location_manager.f90"
module location_manager
    use real_set
    implicit none

    type :: LocationManager
        private
        integer :: value = 0  ! Example value
        type(RealSet) :: locations
    contains
        procedure :: set_locations

        procedure :: get_locations

        procedure :: add_location
    end type LocationManager

    private
    type(LocationManager), save, target :: instance  ! Singleton instance
    logical :: is_initialized = .false.    ! Track initialization
    public :: get_instance

contains

    ! Public accessor function for the singleton instance
    function get_instance() result(single_instance)
        type(LocationManager), pointer :: single_instance

        if (.not. is_initialized) then
            instance = LocationManager()  ! Initialize singleton instance
            is_initialized = .true.
        end if
        single_instance => instance
    end function get_instance

    subroutine set_locations(this, locations)
        class(LocationManager), intent(inout) :: this
        type(RealSet), intent(in) :: locations

        this%locations = locations
    end subroutine set_locations

    function get_locations(this) result(locations)
        class(LocationManager), intent(in) :: this
        type(RealSet) :: locations

        locations = this%locations
    end function get_locations

    subroutine add_location(this, location)
        class(LocationManager), intent(inout) :: this
        real, intent(in) :: location

        call this%locations%add_real_to_set(location)
    end subroutine add_location

end module location_manager
