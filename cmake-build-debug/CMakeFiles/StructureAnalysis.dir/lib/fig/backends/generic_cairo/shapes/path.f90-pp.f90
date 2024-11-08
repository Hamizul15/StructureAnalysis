# 1 "D:/Programming/Fortran/StructureAnalysis/lib/fig/backends/generic_cairo/shapes/path.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/lib/fig/backends/generic_cairo/shapes/path.f90"
module fig_cairo_path
   use cairo
   use fig_shapes
   use fig_canvas
   use fig_cairo_utils
   use fig_path
   private :: parse_string

contains

   subroutine write_path(canva, cr, ph)
      class(base_canvas), intent(inout) :: canva
      type(c_ptr), intent(inout):: cr
      type(path), intent(in) :: ph
      character(len=1), allocatable:: char_array(:)
      real(kind=8), allocatable:: real_array(:)
      integer :: char_count, real_count, char_ind, real_ind
      type(canvas_point) :: cur_p, p1, p2
      type(point) :: temp_p
      real_ind = 1

      cur_p%x = 0
      cur_p%y = 0

      call parse_string(ph%path_string, char_array, real_array, char_count, real_count)

      do char_ind = 1, char_count, 1

        !! TODO need some error handling
         select case (char_array(char_ind))
         case ("M", "m")
            temp_p%x = real_array(real_ind)
            temp_p%y = real_array(real_ind + 1)
            if (char_array(char_ind) == "m") then
               cur_p = to_canvas(temp_p, canva%size) + cur_p
            else
               cur_p = to_canvas(temp_p, canva%size)
            end if
            call cairo_move_to(cr, cur_p%x, cur_p%y)
            real_ind = real_ind + 2
         case ("L", "l")
            temp_p%x = real_array(real_ind)
            temp_p%y = real_array(real_ind + 1)
            if (char_array(char_ind) == "l") then
               cur_p = to_canvas(temp_p, canva%size) + cur_p
            else
               cur_p = to_canvas(temp_p, canva%size)
            end if
            call cairo_line_to(cr, cur_p%x, cur_p%y)
            real_ind = real_ind + 2
         case ("H", "h")
            temp_p%x = real_array(real_ind)
            p1 = to_canvas(temp_p, canva%size)
            if (char_array(char_ind) == "h") then
               cur_p%x = p1%x + cur_p%x
            else
               cur_p%x = p1%x
            end if
            call cairo_line_to(cr, cur_p%x, cur_p%y)
            real_ind = real_ind + 1
         case ("V", "v")
            temp_p%y = real_array(real_ind)
            p1 = to_canvas(temp_p, canva%size)
            if (char_array(char_ind) == "v") then
               cur_p%y = p1%y + cur_p%y
            else
               cur_p%y = p1%y
            end if
            call cairo_line_to(cr, cur_p%x, cur_p%y)
            real_ind = real_ind + 1
         case ("C", "c")
            temp_p%x = real_array(real_ind)
            temp_p%y = real_array(real_ind + 1)
            p1 = to_canvas(temp_p, canva%size)
            temp_p%x = real_array(real_ind + 2)
            temp_p%y = real_array(real_ind + 3)
            p2 = to_canvas(temp_p, canva%size)
            temp_p%x = real_array(real_ind + 4)
            temp_p%y = real_array(real_ind + 5)
            if (char_array(char_ind) == "c") then
               p1 = p1 + cur_p
               p2 = p2 + cur_p
               cur_p = to_canvas(temp_p, canva%size) + cur_p
            else
               cur_p = to_canvas(temp_p, canva%size)
            end if
            call cairo_curve_to(cr, p1%x, p1%y, p2%x, p2%y, cur_p%x, cur_p%y)
            real_ind = real_ind + 6
         case ("Q", "q")
            temp_p%x = real_array(real_ind)
            temp_p%y = real_array(real_ind + 1)
            p1 = to_canvas(temp_p, canva%size)
            temp_p%x = real_array(real_ind + 2)
            temp_p%y = real_array(real_ind + 3)
            p2 = to_canvas(temp_p, canva%size)
            if (char_array(char_ind) == "q") then
               p1 = p1 + cur_p
               p2 = p2 + cur_p
            end if
            call quad_to(cr, cur_p%x, cur_p%y, p1%x, p1%y, p2%x, p2%y)
            cur_p = p2
            real_ind = real_ind + 4
         case ("A", 'a')
            temp_p%x = real_array(real_ind)
            temp_p%y = real_array(real_ind + 1)
            p1 = to_canvas(temp_p, canva%size)
            temp_p%x = real_array(real_ind + 5)
            temp_p%y = real_array(real_ind + 6)
            p2 = to_canvas(temp_p, canva%size)
            if (char_array(char_ind) == "a") then
               p1 = p1 + cur_p
               p2 = p2 + cur_p
            end if
            call arc_to(cr, cur_p%x, cur_p%y, p1%x, p1%y, &
                        real_array(real_ind + 2), real_array(real_ind + 3) > 0.1, real_array(real_ind + 4) > 0.1, p2%x, &
                        p2%y)
            cur_p = p2
            real_ind = real_ind + 7
         case ("Z", "z")
            real_ind = real_ind + 1
            call cairo_close_path(cr)
         end select

      end do

      call fill(cr, ph)
      call stroke(cr, ph)

   end subroutine write_path

   subroutine parse_string(input_str, char_array, real_array, char_count, real_count)
      implicit none
      character(len=*), intent(in) :: input_str
      character(len=1), allocatable, intent(out) :: char_array(:)
      real(kind=8), allocatable, intent(out) :: real_array(:)
      integer, intent(out) :: char_count, real_count
      integer :: i, length
      character(len=1) :: current_char
      character(len=20) :: temp_string
      logical :: is_num

      ! Initialize variables
      char_count = 0
      real_count = 0
      length = len_trim(input_str)

      allocate (char_array(length))  ! Allocate char_array based on input length
      allocate (real_array(length))  ! Allocate real_array based on input length

      i = 1
      do while (i <= length)
         current_char = input_str(i:i)

         if (current_char == '-') then
            ! Detecting a negative number
            temp_string = '-'
            i = i + 1
            current_char = input_str(i:i)
            is_num = .true.
            do while (is_num .and. i <= length)
               if ((current_char >= '0' .and. current_char <= '9') .or. current_char == '.') then
                  temp_string = trim(temp_string)//current_char
                  i = i + 1
                  if (i <= length) then
                     current_char = input_str(i:i)
                  else
                     is_num = .false.
                  end if
               else
                  is_num = .false.
               end if
            end do
            if (len_trim(temp_string) > 0) then
               real_count = real_count + 1
               read (temp_string, *) real_array(real_count)
            end if
         else if ((current_char >= '0' .and. current_char <= '9') .or. current_char == '.') then
            ! Start of a non-negative number
            temp_string = ''
            is_num = .true.
            do while (is_num .and. i <= length)
               if ((current_char >= '0' .and. current_char <= '9') .or. current_char == '.') then
                  temp_string = trim(temp_string)//current_char
                  i = i + 1
                  if (i <= length) then
                     current_char = input_str(i:i)
                  else
                     is_num = .false.
                  end if
               else
                  is_num = .false.
               end if
            end do
            if (len_trim(temp_string) > 0) then
               real_count = real_count + 1
               read (temp_string, *) real_array(real_count)
            end if
         else if ((current_char >= 'A' .and. current_char <= 'Z') .or. (current_char >= 'a' .and. current_char <= 'z')) then
            char_count = char_count + 1
            char_array(char_count) = current_char
            i = i + 1
         else
            i = i + 1
         end if
      end do

   end subroutine parse_string
end module fig_cairo_path
