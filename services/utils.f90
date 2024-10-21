! Created by hamiz on 10/21/2024.
module utils
    contains

        logical function is_whitespace(text)
            character(LEN=*) :: text
            is_whitespace = .false.
            write(*,*), len(text)
            do i = 1, len(text)
                if (text(i:i) /= ' ') then
                    is_whitespace = .true.
                    exit
                end if
            end do
        end function is_whitespace

end module utils