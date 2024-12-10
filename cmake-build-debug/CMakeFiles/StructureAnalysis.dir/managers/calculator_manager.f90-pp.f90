# 1 "D:/Programming/Fortran/StructureAnalysis/managers/calculator_manager.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/managers/calculator_manager.f90"
! Created by hamiz on 11/28/2024.
module calculator_manager
    use supports_module
    use input_manager
    use calculator_service, only : Calculator, CalculatorUndetermined, calculate_bro => calculate, init
    use calculator_determined_service, only : CalculatorDetermined, calculate_sis => calculate
    implicit none

    type :: CalculatorManager

        contains
        procedure :: calculate

    end type CalculatorManager

    contains

    subroutine calculate(this, inp)
        implicit none
        class(CalculatorManager), intent(in) :: this
        type(Input), intent(in) :: inp
        type(SupportSet) :: sup_set
        class(Calculator), allocatable :: calc
        integer :: number_of_reactions

        sup_set = inp%get_supports()
        number_of_reactions = sup_set%sum_of_number_of_reactions()

        if(number_of_reactions == 3) then !it means statically determinate
            allocate(CalculatorDetermined :: calc)
        else if (number_of_reactions < 3) then
            print *, "Kondisi gelagar tidak stabil!"
        else if (number_of_reactions > 3) then
            !allocate(CalculatorUndetermined :: calc)
            print *, "Terdeteksi Struktur Tidak Tentu! Program kami belum mendukung untuk memecahkan tersebut!"
        end if

        if (allocated(calc)) then
            call calc%init(inp)
            call calc%calculate()
        end if
    end subroutine calculate

end module calculator_manager
