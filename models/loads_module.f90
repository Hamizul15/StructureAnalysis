! Created by hamiz on 10/21/2024.

module loads_module
    use input_service
    implicit none

    private :: get_types

    type :: Load
        private
        real :: start_location, end_location, start_load, end_load

       contains
            !setter
            procedure :: set_start_location
            procedure :: set_start_load
            procedure :: set_end_location
            procedure :: set_end_load

           !getter
            procedure :: get_start_location
            procedure :: get_end_location
            procedure :: get_start_load
            procedure :: get_end_load
    end type Load

    type, extends(Load) :: Point
    contains
        !setter
        procedure :: set_start_location => set_point_location
        procedure :: set_end_location => set_point_location
        procedure :: set_start_load => set_point_load
        procedure :: set_end_load => set_point_load

        !getter
        procedure :: get_start_location => get_point_location
        procedure :: get_end_location => get_point_location
        procedure :: get_start_load => get_point_load
        procedure :: get_end_load => get_point_load
    end type Point

    type, extends(Load) :: Moment
    contains
        !setter
        procedure :: set_start_location => set_moment_location
        procedure :: set_end_location => set_moment_location
        procedure :: set_start_load => set_moment_load
        procedure :: set_end_load => set_moment_load

        !getter
        procedure :: get_start_location => get_moment_location
        procedure :: get_end_location => get_moment_location
        procedure :: get_start_load => get_moment_load
        procedure :: get_end_load => get_moment_load
    end type Moment

    type, extends(Load) :: Distributed
    contains
        !setter
        !procedure :: set_start_location => set_distributed_start_location
        !procedure :: set_end_location => set_distributed_end_location
        !procedure :: set_start_load => set_distributed_start_load
        !procedure :: set_end_load => set_distributed_end_load

        !getter
        !procedure :: get_start_location => get_distributed_start_location
        !procedure :: get_end_location => get_distributed_end_location
        !procedure :: get_start_load => get_distributed_start_load
        !procedure :: get_end_load => get_distributed_end_load
    end type Distributed

    interface assignment(=)
        module procedure assign_load
    end interface

    contains

        function new_load() result(new_ld)
            implicit none
            class(Load), allocatable :: new_ld
            integer :: type_

            type_ = get_type()
            if(type_ == 1) then
                allocate(Point :: new_ld)
            elseif (type_ == 2) then
                allocate(Moment :: new_ld)
            else
                allocate(Distributed :: new_ld)
            end if

           if((type_ /= size(get_types()))) then
              call new_ld%set_start_location(10.0)
           end if

        end function new_load

        function get_type() result(type_)
            integer :: type_, i
            CHARACTER(len=10), DIMENSION(3) :: types

            types = get_types()

            WRITE (*, '(A)') 'Silahkan pilih tipe beban'
            DO i = 1, SIZE(types)
                PRINT '(I1.0, A, A)', i, '. ', types(i)
            END DO

            type_ = get_integer("Masukkan tipe beban: ", 1, size(types))
        end function get_type

        function get_types() result(types)
            CHARACTER(len=10), DIMENSION(3) :: types
            types(1) = 'Point'
            types(2) = 'Moment'
            types(3) = 'Distributed'
        end function get_types

        !Setter
        subroutine set_start_location(this, new_location)
            class(Load), intent(inout) :: this
            real, intent(in) :: new_location

            this%start_location = new_location
        end subroutine set_start_location

        subroutine set_end_location(this, new_location)
            class(Load), intent(inout) :: this
            real, intent(in) :: new_location

            this%end_location = new_location
        end subroutine set_end_location

        subroutine set_start_load(this, new_load)
            class(Load), intent(inout) :: this
            real, intent(in) :: new_load

            this%start_load = new_load
        end subroutine set_start_load

        subroutine set_end_load(this, new_load)
            class(Load), intent(inout) :: this
            real, intent(in) :: new_load

            this%end_load = new_load
        end subroutine set_end_load

        !Getter
        function get_start_location(this) result(loc)
            class(Load), intent(in) :: this
            real :: loc

            loc = this%start_location
        end function get_start_location

        function get_end_location(this) result(loc)
            class(Load), intent(in) :: this
            real :: loc

            loc = this%end_location
        end function get_end_location

        function get_start_load(this) result(loc)
            class(Load), intent(in) :: this
            real :: loc

            loc = this%start_load
        end function get_start_load

        function get_end_load(this) result(loc)
            class(Load), intent(in) :: this
            real :: loc

            loc = this%end_load
        end function get_end_load


        !Point
        subroutine set_point_location(this, new_location)
            class(Point), intent(inout) :: this
            real, intent(in) :: new_location

            this%start_location = new_location
            this%end_location = new_location
        end subroutine set_point_location

        function get_point_location(this) result(loc)
            class(Point), intent(in) :: this
            real :: loc

            loc = this%start_location
            if (this%start_location /= this%end_location) then
                loc = -1;
            end if
        end function get_point_location

        subroutine set_point_load(this, new_load)
            class(Point), intent(inout) :: this
            real, intent(in) :: new_load

            this%start_load = new_load
            this%end_load = new_load
        end subroutine set_point_load

        function get_point_load(this) result(load)
            class(Point), intent(in) :: this
            real :: load

            load = this%start_location
            if (this%start_load /= this%end_load) then
                load = -1;
            end if
        end function get_point_load


        !Moment
        subroutine set_moment_location(this, new_location)
            class(Moment), intent(inout) :: this
            real, intent(in) :: new_location

            this%start_location = new_location
            this%end_location = new_location
        end subroutine set_moment_location

        function get_moment_location(this) result(loc)
            class(Moment), intent(in) :: this
            real :: loc

            loc = this%start_location
            if (this%start_location /= this%end_location) then
                loc = -1;
            end if
        end function get_moment_location

        subroutine set_moment_load(this, new_load)
            class(Moment), intent(inout) :: this
            real, intent(in) :: new_load

            this%start_load = new_load
            this%end_load= new_load
        end subroutine set_moment_load

        function get_moment_load(this) result(load)
            class(Moment), intent(in) :: this
            real :: load

            load = this%start_load
            if (this%start_load /= this%end_load) then
                load = -1;
            end if
        end function get_moment_load


        ! Custom assignment operator for the Load type
        subroutine assign_load(lhs, rhs)
            class(Load), intent(out) :: lhs
            class(Load), intent(in) :: rhs
            lhs%start_location = rhs%start_location
            lhs%end_location = rhs%end_location
            lhs%start_load = rhs%start_load
            lhs%end_load = rhs%end_load
        end subroutine assign_load

end module loads_module