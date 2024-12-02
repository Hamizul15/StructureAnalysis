! Created by hamiz on 11/27/2024.
module result_module
    use resultload_arraylist

    type :: Result

        private
        type(ResultLoadArrayList) :: reactions
        type(ResultLoadArrayList) :: moment_reactions
        type(ResultLoadArrayList), allocatable :: moments(:)
        type(ResultLoadArrayList), allocatable :: sheers(:)

    contains
        procedure :: set_reactions, set_moment_reactions, set_moments, set_sheers
        procedure :: get_moments, get_sheers, get_reactions, get_moment_reactions

    end type Result

contains

    subroutine set_moments(this, moments)
        class(Result), intent(inout) :: this
        type(ResultLoadArrayList), allocatable :: moments(:)

        this%moments = moments
    end subroutine set_moments

    subroutine set_sheers(this, sheers)
        class(Result), intent(inout) :: this
        type(ResultLoadArrayList), allocatable :: sheers(:)

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

    function get_sheers(this) result(shir)
        class(Result), intent(in) :: this
        type(ResultLoadArrayList), allocatable :: shir(:)

        shir = this%sheers
    end function get_sheers

    function get_moments(this) result(mom)
        class(Result), intent(in) :: this
        type(ResultLoadArrayList), allocatable :: mom(:)

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