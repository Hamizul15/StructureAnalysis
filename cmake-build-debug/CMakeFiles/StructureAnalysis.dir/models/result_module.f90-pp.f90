# 1 "D:/Programming/Fortran/StructureAnalysis/models/result_module.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/models/result_module.f90"
! Created by hamiz on 11/27/2024.
module result_module
    use resultload_arraylist

    type :: Result

        private
        real :: ra, rb, ma
        type(ResultLoadArrayList) :: moments
        type(ResultLoadArrayList) :: sheers

        contains
        procedure set_ra, set_rb, set_ma
        procedure :: get_ra, get_rb, get_ma, get_moments, get_sheers

    end type Result

    contains

    subroutine set_ra(this, ra)
        class(Result), intent(inout) :: this
        real :: ra

        this%ra = ra
    end subroutine set_ra

    subroutine set_rb(this, rb)
        class(Result), intent(inout) :: this
        real :: rb

        this%rb = rb
    end subroutine set_rb

    subroutine set_ma(this, ma)
        class(Result), intent(inout) :: this
        real :: ma

        this%ma = ma
    end subroutine set_ma

    function get_ra(this) result(ra_)
        class(Result), intent(in) :: this
        real :: ra_

        ra_ = this%ra;
    end function get_ra

    function get_rb(this) result(rb_)
        class(Result), intent(in) :: this
        real :: rb_

        rb_ = this%rb;
    end function get_rb

    function get_ma(this) result(ma_)
        class(Result), intent(in) :: this
        real :: ma_

        ma_ = this%ma;
    end function get_ma

    function get_sheers(this) result(shir)
        class(Result), intent(in) :: this
        type(ResultLoadArrayList) :: shir

        shir = this%sheers
    end function get_sheers

    function get_moments(this) result(mom)
        class(Result), intent(in) :: this
        type(ResultLoadArrayList) :: mom

        mom = this%moments
    end function get_moments

end module result_module