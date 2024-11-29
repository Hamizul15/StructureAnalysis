! Created by hamiz on 11/29/2024.
module location_interval_module

    type :: LocationInterval
        private
        real :: start, end

    contains

        procedure :: set_start, set_end
        procedure :: get_start, get_end

    end type LocationInterval

    interface assignment(=)
        module procedure assign_location_interval
    end interface

    contains

        subroutine set_start(this, start)
            class(LocationInterval), intent(inout) :: this
            real :: start

            this%start = start
        end subroutine set_start

        subroutine set_end(this, end)
            class(LocationInterval), intent(inout) :: this
            real :: end

            this%end = end
        end subroutine set_end

        function get_start(this) result(start)
            class(LocationInterval), intent(inout) :: this
            real :: start

            start = this%start
        end function get_start

        function get_end(this) result(end)
            class(LocationInterval), intent(inout) :: this
            real :: end

            end = this%end
        end function get_end



        ! Custom assignment operator for the LocationInterval type
        subroutine assign_location_interval(lhs, rhs)
            class(LocationInterval), intent(out), allocatable :: lhs
            class(LocationInterval), intent(in) :: rhs
            lhs%start = rhs%start
            lhs%end = rhs%end
        end subroutine assign_location_interval

end module location_interval_module