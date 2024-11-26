! Created by hamiz on 10/21/2024.
module input_service
    contains
        integer function get_integer(message, min, max)
            implicit none
            integer :: the_integer, validInput
            integer, optional :: min, max
            integer :: amin, amax
            character(LEN=*), optional :: message

            amin = huge(the_integer) * -1
            amax = huge(the_integer)

            if(present(min)) amin = min
            if(present(max)) amax = max

            do
                if (.not.(present(message))) then
                    write(*, '(A)', ADVANCE='NO') 'Please enter an integer: '
                else
                    write(*, '(A)', ADVANCE='NO') message
                end if
                READ(*, *, IOSTAT=validInput) the_integer
                if (validInput == 0) then
                    if(the_integer < amin) then
                        print *, "the minimum input is ", amin
                    else if (the_integer > amax) then
                        print *,  "the maximum input is ", amax
                    else
                        get_integer = the_integer
                        exit
                    end if
                else
                    print *, 'Invalid input. Please try again.'
                end if
            end do
        end function get_integer


        !fungsi ini mengatur tentang input real
        real function get_real(message, min, max)
            implicit none
            real :: the_real
            integer :: validInput
            real, optional :: min, max
            real :: amin, amax
            character(LEN=*), optional :: message

            amin = huge(the_real) * -1
            amax = huge(the_real)

            if(present(min)) amin = min
            if(present(max)) amax = max

            do
                if (.not.(present(message))) then
                    write(*, '(A)', ADVANCE='NO') 'Please enter a real: '
                else
                    write(*, '(A)', ADVANCE='NO') message
                end if
                READ(*, *, IOSTAT=validInput) the_real
                if (validInput == 0) then
                    if(the_real < amin) then
                        print *, "the minimum input is ", amin
                    else if (the_real > amax) then
                        print *,  "the maximum input is ", amax
                    else
                        get_real = the_real
                        exit
                    end if
                else
                    print *, 'Invalid input. Please try again.'
                end if
            end do
        end function get_real

end module input_service