# 1 "D:/Programming/Fortran/StructureAnalysis/lib/fig/backends/generic_cairo/cairo_backend.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/lib/fig/backends/generic_cairo/cairo_backend.f90"
module fig_cairo
   use cairo
   use cairo_enums
   use cairo_types
   use cairo_extra
   use fig_cairo_utils, only: set_rgba
   use fig_canvas
   use fig_shapes
   use fig_drawing
   use fig_config
   use fig_cairo_circle, only: write_circle
   use fig_cairo_ellipse, only: write_ellipse
   use fig_cairo_line, only: write_line
   use fig_cairo_arc, only: write_arc
   use fig_cairo_path, only: write_path
   use fig_cairo_poly, only: write_polygon, write_polyline
   use fig_cairo_rect, only: write_rectangle
   use fig_cairo_text, only: write_text
   use fig_cairo_triangle, only: write_triangle
   use fig_path
   use fig_rgb
   implicit none

   private

   type, extends(base_canvas), public :: cairo_canvas
      type(c_ptr) :: surface
      type(c_ptr) :: cairo
   contains
      procedure :: init => init_cairo
      procedure :: destroy
      procedure :: apply_shapes
      procedure :: draw_shape => cairo_write_shape
   end type cairo_canvas
contains

   subroutine init_cairo(this, width, height, title)
      class(cairo_canvas), intent(inout) :: this
      integer, intent(in) :: width, height
      character(len=*), intent(in) :: title

      this%size%width = width
      this%size%height = height
      this%title = title
   end subroutine init_cairo

   subroutine destroy(this)
      class(cairo_canvas), intent(inout) :: this

      call cairo_destroy(this%cairo)
      call cairo_surface_destroy(this%surface)
   end subroutine destroy

   subroutine cairo_write_shape(canva, sh)
      class(cairo_canvas), intent(inout) :: canva
      class(shape), intent(in) :: sh

      select type (sh)
      type is (circle)
         call write_circle(canva, canva%cairo, sh)
      type is (ellipse)
         call write_ellipse(canva, canva%cairo, sh)
      type is (rectangle)
         call write_rectangle(canva, canva%cairo, sh)
      type is (line)
         call write_line(canva, canva%cairo, sh)
      type is (triangle)
         call write_triangle(canva, canva%cairo, sh)
      type is (text)
         call write_text(canva, canva%cairo, sh)
      type is (path)
         call write_path(canva, canva%cairo, sh)
      type is (polyline)
         call write_polyline(canva, canva%cairo, sh)
      type is (polygon)
         call write_polygon(canva, canva%cairo, sh)
      type is (arc)
         call write_arc(canva, canva%cairo, sh)
      end select

   end subroutine cairo_write_shape

   subroutine apply_shapes(canva, draw)
      class(cairo_canvas), intent(inout) :: canva
      type(drawing), intent(in):: draw
      integer :: i
      call set_rgba(canva%cairo, draw%background)
      call cairo_paint(canva%cairo)

      do i = 1, draw%shape_count
         call cairo_write_shape(canva, draw%shapes(i)%sh)
      end do

   end subroutine apply_shapes

end module fig_cairo

