! Created by hamiz on 10/21/2024.
module loads_module
    use input_service
    use location_manager
    implicit none

    integer, parameter :: NUMBER_OF_LOADS = 3, POINT_ = 1, MOMENT_ = 2, DISTRIBUTED_ = 3

    private :: get_types, get_type

    type :: Load
        private
        real :: start_location, end_location, start_load, end_load
        integer :: type

       contains
            private
            procedure :: set_type

            !setter
            procedure, public :: set_start_location
            procedure, public :: set_start_load
            procedure, public :: set_end_location
            procedure, public :: set_end_load

           !getter
            procedure, public :: get_start_location
            procedure, public :: get_end_location
            procedure, public :: get_start_load
            procedure, public :: get_end_load

            procedure, public :: get_total_load, get_type, get_actual_location
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
        procedure :: get_total_load => get_distributed_total_load

    end type Distributed

    interface assignment(=)
        module procedure assign_load
    end interface

    contains

        function new_load(max) result(new_ld)
            implicit none
            class(Load), allocatable :: new_ld
            real, optional ::  max
            integer :: type_

            type_ = get_choosen_type()
            if(type_ == 1) allocate(Point :: new_ld)
            if(type_ == 2) allocate(Moment :: new_ld)
            if(type_ == 3) allocate(Distributed :: new_ld)
            call new_ld%set_type(type_)

            select type(stored => new_ld)
            type is (Distributed)
                call new_ld%set_start_location(get_real("Masukkan posisi awal beban terpusat: ", 0.0, max))
                call new_ld%set_end_location(get_real("Masukkan posisi akhir beban terpusat: ", new_ld%get_start_location(), max))
                call new_ld%set_start_load(get_real("Masukkan beban terpusat awal: "))

                if(new_ld%get_start_load() > 0) then
                    call new_ld%set_end_load(get_real("Masukkan beban terpusat akhir: ", 0.0))
                else if (new_ld%get_start_load() < 0) then
                    call new_ld%set_end_load(get_real("Masukkan beban terpusat akhir: ", huge(max) * -1, 0.0))
                else
                    call new_ld%set_end_load(get_real("Masukkan beban terpusat akhir: "))
                end if

                call add_location(new_ld%get_start_location())
                call add_location(new_ld%get_end_location())

            class default
                call new_ld%set_start_location(get_real("Masukkan posisi: ", 0.0, max))
                call new_ld%set_start_load(get_real("Masukkan beban: "))
                call add_location(new_ld%get_start_location())
            end select

        end function new_load

        function get_choosen_type() result(type_)
            integer :: type_, i
            CHARACTER(len=10), DIMENSION(NUMBER_OF_LOADS) :: types

            types = get_types()

            WRITE (*, '(A)') 'Silahkan pilih tipe beban'
            DO i = 1, SIZE(types)
                PRINT '(I1.0, A, A)', i, '. ', types(i)
            END DO

            type_ = get_integer("Masukkan tipe beban: ", 1, size(types))
        end function get_choosen_type

        function get_types() result(types)
            CHARACTER(len=10), DIMENSION(NUMBER_OF_LOADS) :: types
            types(POINT_) = 'Point'
            types(MOMENT_) = 'Moment'
            types(DISTRIBUTED_) = 'Distributed'
        end function get_types

        !Setter
        subroutine set_type(this, typee)
            class(Load), intent(inout) :: this
            integer :: typee

            this%type = typee
        end subroutine set_type

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
        function get_type(this) result(tipe)
            class(Load), intent(in) :: this
            integer :: tipe

            tipe = this%type
        end function get_type

        function get_total_load(this) result(total)
            class(Load), intent(in) :: this
            real :: total, length

            if(this%get_type() == 3) then
                length = this%get_end_location() - this%get_start_location()
                total = (this%get_start_load() + this%get_end_load()) * length / 2
            else
                total = this%get_start_load()
            end if

        end function get_total_load

        function get_actual_location(this) result(loc)
            class(Load), intent(in) :: this
            real :: loc, length, first_part, second_part, third_part

            if(this%get_type() == 3) then
                length = this%get_end_location() - this%get_start_location()
                first_part = (this%get_start_load() + 2 * this%get_end_load()) * length
                second_part = 3 * (this%get_start_load() + this%get_end_load())
                !third_part = 3 * (this%get_start_load() + this%get_end_load())
                loc = first_part / second_part
            else
                loc = this%get_start_location()
            end if

        end function get_actual_location

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


        !Distributed
        function get_distributed_total_load(this) result(total)
            class(Distributed), intent(in) :: this
            real :: length, total

            length = this%get_end_location() - this%get_start_location()
            total = (this%get_start_load() + this%get_end_load()) * length / 2
        end function get_distributed_total_load


        ! Custom assignment operator for the Load type
        subroutine assign_load(lhs, rhs)
            class(Load), intent(out), allocatable :: lhs
            class(Load), intent(in) :: rhs
            lhs%start_location = rhs%start_location
            lhs%end_location = rhs%end_location
            lhs%start_load = rhs%start_load
            lhs%end_load = rhs%end_load
            lhs%type = rhs%type
        end subroutine assign_load

end module loads_module