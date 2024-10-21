! Created by hamiz on 10/22/2024.

module supports_module
    implicit none

    type :: Support

        private
        integer :: number_of_reaction
        real :: location
    end type Support

    type, extends(Supoort) :: Pin

    end type Pin

end module supports_module