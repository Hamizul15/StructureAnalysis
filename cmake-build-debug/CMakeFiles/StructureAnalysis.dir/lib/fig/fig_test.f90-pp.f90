# 1 "D:/Programming/Fortran/StructureAnalysis/lib/fig/fig_test.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/lib/fig/fig_test.f90"
module fig_test
   use cairo_extra
   use fig_bitmap
   use fig_config
   use fig_rgb_color_constants
   use fig_rgb
   implicit none

contains
   subroutine test_svg(canvas_name, err)
      character(len=*), intent(in) :: canvas_name
      integer, intent(out) :: err
      integer :: status
      character(len=256) :: current_file, expected_file, diff_command, diff_file
      current_file = canvas_name//".svg"
      expected_file = "test/expected/"//canvas_name//".svg"
      diff_file = canvas_name//"_svg.diff"
      diff_command = "diff -u "//trim(current_file)//" "//trim(expected_file)
      call execute_command_line(trim(diff_command//" > /dev/null 2>&1"), exitstat=status)

      if (status == 0) then
         print *, canvas_name, " Svg test passed."
      else
         call execute_command_line(trim(diff_command)//">"//diff_file)
         print *, "!!Svg Test failed."//"See differences in file: "//trim(diff_file)
         err = 1
      end if
   end subroutine test_svg

   subroutine test_bitmap(canvas_name, err)
      character(len=*), intent(in) :: canvas_name
      integer, intent(out) :: err
      integer :: status
      character(len=256) :: current_file, expected_file, diff_command, diff_file
      current_file = canvas_name//".png"
      expected_file = "test/expected/"//canvas_name//".png"
      diff_file = canvas_name//"_diff.png"
      diff_command = "./odiff "//trim(current_file)//" "//trim(expected_file)
      call execute_command_line(trim(diff_command//" > /dev/null 2>&1"), exitstat=status)

      if (status == 0) then
         print *, canvas_name, " bitmap test passed."
      else
         call execute_command_line(trim(diff_command)//" "//diff_file)
         print *, "!!Bitmap Test failed."//"See differences in file: "//trim(diff_file)
         err = 1
      end if

   end subroutine test_bitmap

   subroutine test_both(canvas_name)
      character(len=*), intent(in) :: canvas_name







   end subroutine test_both

end module fig_test

