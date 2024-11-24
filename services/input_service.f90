! Created by hamiz on 10/21/2024.
module input_service
    contains
        integer function get_integer(message, min, max)
            implicit none
            integer :: the_integer, validInput
            integer, optional :: min, max
            character(LEN=*), optional :: message
            do
                if (.not.(present(message))) then
                    write(*, '(A)', ADVANCE='NO') 'Please enter an integer:'
                else
                    write(*, '(A)', ADVANCE='NO') message
                end if
                READ(*, *, IOSTAT=validInput) the_integer
                if (validInput == 0) then
                    if(present(min).and.(the_integer < min)) then
                        print *, "the minimum input is ", min
                    else if (present(max).and.(the_integer > max)) then
                        print *,  "the maximum input is ", max
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
        integer function get_real(message, min, max)
            implicit none
            real :: the_real
            real, optional :: min, max
            integer :: validInput
            character(LEN=*), optional :: message
            do
                if (.not.(present(message))) then
                    write(*, '(A)', ADVANCE='NO') 'Please enter an integer:'
                else
                    write(*, '(A)', ADVANCE='NO') message
                end if
                READ(*, *, IOSTAT=validInput) the_real
                if (validInput == 0) then
                    if(present(min).and.(the_real < min)) then
                        print *, "the minimum input is ", min
                    else if (present(max).and.(the_real > max)) then
                        print *,  "the maximum input is ", max
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