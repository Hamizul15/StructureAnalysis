# 1 "D:/Programming/Fortran/StructureAnalysis/lib/stdlib/stdlib_system.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/lib/stdlib/stdlib_system.F90"
module stdlib_system
use, intrinsic :: iso_c_binding, only : c_int, c_long
implicit none
private
public :: sleep

interface
# 18 "D:/Programming/Fortran/StructureAnalysis/lib/stdlib/stdlib_system.F90"
integer(c_int) function usleep(usec) bind (C)
!! version: experimental
!!
!! int usleep(useconds_t usec);
!! https://linux.die.net/man/3/usleep
import c_int
integer(c_int), value, intent(in) :: usec
end function usleep

end interface

contains

subroutine sleep(millisec)
!! version: experimental
!!
integer, intent(in) :: millisec
integer(c_int) :: ierr





!! Linux, Unix, MacOS, MSYS2, ...
ierr = usleep(int(millisec * 1000, c_int))
if (ierr/=0) error stop 'problem with usleep() system call'



end subroutine sleep

end module stdlib_system
