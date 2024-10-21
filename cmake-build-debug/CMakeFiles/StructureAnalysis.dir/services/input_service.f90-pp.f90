# 1 "D:/Programming/Fortran/StructureAnalysis/services/input_service.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/services/input_service.f90"
! Created by hamiz on 10/21/2024.
module input_service
    contains
        integer function get_integer(message)
            implicit none
            integer :: theinteger, validInput
            character(LEN=*), optional :: message
            do
                if (.not.(present(message))) then
                    write(*, '(A)', ADVANCE='NO') 'Please enter an integer:'
                else
                    write(*, '(A)', ADVANCE='NO') message
                end if
                READ(*, *, IOSTAT=validInput) theinteger
                if (validInput == 0) then
                    get_integer = theinteger
                    exit
                else
                    print *, 'Invalid input. Please try again.'
                end if
            end do
        end function get_integer


        !fungsi ini mengatur tentang input real
        integer function get_real(message)
            implicit none
            real :: thereal
            integer :: validInput
            character(LEN=*), optional :: message
            do
                if (.not.(present(message))) then
                    write(*, '(A)', ADVANCE='NO') 'Please enter an integer:'
                else
                    write(*, '(A)', ADVANCE='NO') message
                end if
                READ(*, *, IOSTAT=validInput) thereal
                if (validInput == 0) then
                    get_real = thereal
                    exit
                else
                    print *, 'Invalid input. Please try again.'
                end if
            end do
        end function get_real

end module input_service
