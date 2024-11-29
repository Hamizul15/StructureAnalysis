# 1 "D:/Programming/Fortran/StructureAnalysis/main.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/main.f90"
program structure_analysis
    use input_manager
    use calculator_manager
    use location_manager
    use location_interval_arraylist
    implicit none

    type(Input) :: inp
    type(CalculatorManager) :: cacl
    character :: continue_option
    type(LocationIntervalArrayList) :: intervals
    type(LocationInterval) :: current_inter
    integer :: i

    do
        call construct_location_manager()
        call inp%start()

        intervals = get_intervals()
        do i = 1, intervals%get_size()
            current_inter = intervals%get_location_lnterval(i)
            print *, current_inter%get_start(), " <= x <= ", current_inter%get_end()
        end do

        !call cacl%calculate(inp)

        continue_option = get_character("Menghitung lagi(input Y) atau Berhenti(input sesuatu): ")
        if((continue_option /= 'Y').and.(continue_option /= 'y')) then
            exit
        end if
        call inp%dispose()
        call dispose_location_manager()
    end do

end program
