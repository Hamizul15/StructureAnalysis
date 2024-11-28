! Created by hamiz on 11/27/2024.
module result_module
    use resultload_arraylist

    type :: Result

        private
        !type(ResultLoad) :: ra, rb, ma
        type(ResultLoadArrayList) :: reactions
        type(ResultLoadArrayList) :: moment_reactions
        type(ResultLoadArrayList) :: moments
        type(ResultLoadArrayList) :: sheers

        contains
        !procedure :: set_ra, set_rb, set_ma
        !procedure :: get_ra, get_rb, get_ma
        procedure :: set_reactions, set_moment_reactions, set_moments, set_sheers
        procedure :: get_moments, get_sheers, get_reactions, get_moment_reactions

    end type Result

    contains

    !subroutine set_ra(this, ra)
    !    class(Result), intent(inout) :: this
    !    type(ResultLoad) :: ra

    !    this%ra = ra
    !end subroutine set_ra

    !subroutine set_rb(this, rb)
    !    class(Result), intent(inout) :: this
    !    type(ResultLoad) :: rb

    !    this%rb = rb
    !end subroutine set_rb

    !subroutine set_ma(this, ma)
    !    class(Result), intent(inout) :: this
    !    type(ResultLoad) :: ma

    !    this%ma = ma
    !end subroutine set_ma

    subroutine set_moments(this, moments)
        class(Result), intent(inout) :: this
        type(ResultLoadArrayList) :: moments

        this%moments = moments
    end subroutine set_moments

    subroutine set_sheers(this, sheers)
        class(Result), intent(inout) :: this
        type(ResultLoadArrayList) :: sheers

        this%sheers = sheers
    end subroutine set_sheers

    subroutine set_reactions(this, reactions)
        class(Result), intent(inout) :: this
        type(ResultLoadArrayList) :: reactions

        this%reactions = reactions
    end subroutine set_reactions

    subroutine set_moment_reactions(this, mreactions)
        class(Result), intent(inout) :: this
        type(ResultLoadArrayList) :: mreactions

        this%moment_reactions = mreactions
    end subroutine set_moment_reactions



    !Getter
    !function get_ra(this) result(ra_)
    !    class(Result), intent(in) :: this
    !    type(ResultLoad) :: ra_

    !    ra_ = this%ra;
    !end function get_ra

    !function get_rb(this) result(rb_)
    !    class(Result), intent(in) :: this
    !    type(ResultLoad) :: rb_

    !    rb_ = this%rb;
    !end function get_rb

    !function get_ma(this) result(ma_)
    !    class(Result), intent(in) :: this
    !    type(ResultLoad) :: ma_

    !    ma_ = this%ma;
    !end function get_ma

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

    function get_moment_reactions(this) result(mreactions)
        class(Result), intent(in) :: this
        type(ResultLoadArrayList) :: mreactions

        mreactions = this%moment_reactions
    end function get_moment_reactions

    function get_reactions(this) result(reactions)
        class(Result), intent(in) :: this
        type(ResultLoadArrayList) :: reactions

        reactions = this%reactions
    end function get_reactions

end module result_module