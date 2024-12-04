# 1 "D:/Programming/Fortran/StructureAnalysis/services/io_service.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/services/io_service.f90"
! Created by hamiz on 12/4/2024.
module io_service
    use loads_module
    use load_arraylist, only : Load, LoadArrayList, get_loads_size => get_size
    use supports_module
    use support_hashmap, only: SupportHashMap, get_supports_size => get_size, get_support_location => get_location
    use result_module, only : Result, ResultLoad, ResultLoadArrayList
    use location_interval_arraylist
    implicit none

    integer, parameter :: INPUT_ = 10, OUTPUT_ = 20

    private :: get_type_load, get_type_support, create_output_directory

    public :: write_input
    public :: write_output

    contains

    subroutine write_input(length, loads, supports)
        real, intent(inout) :: length
        type(LoadArrayList), intent(inout) :: loads
        type(SupportHashMap), intent(inout) :: supports
        character(len=100) :: filename
        type(Load) :: current_load
        type(Support) :: current_sup
        integer :: i

        filename = './output/input.txt'

        call create_output_directory()
        open(unit=INPUT_, file=filename, status='unknown')

        !panjang beam
        write(INPUT_, '(A)', advance='no') "Panjang Beam: "
        write(INPUT_, '(F16.3)') length
        write(INPUT_, '(A)') ""

        !loads
        write(INPUT_, '(A)') "Beban: "
        write(INPUT_, '(A1)', advance='no') "("
        write(INPUT_, '(A12)', advance='no') "Tipe Beban"
        write(INPUT_, '(A2)', advance='no') ") "
        write(INPUT_, '(A16)', advance='no') "Lokasi"
        write(INPUT_, '(A8)', advance='no') "------>"
        write(INPUT_, '(A16)') "Beban"
        do i = 1, loads%get_size()
            current_load = loads%get_load(i)

            write(INPUT_, '(A1)', advance='no') "("
            write(INPUT_, '(A12)', advance='no') get_type_load(current_load%get_type())
            write(INPUT_, '(A2)', advance='no') ") "

            if(current_load%get_type() == DISTRIBUTED_) then
                write(INPUT_, '(F16.3)', advance='no') current_load%get_start_location()
                write(INPUT_, '(A)', advance='no') "-"
                write(INPUT_, '(F16.3)', advance='no') current_load%get_end_location()
                write(INPUT_, '(A8)', advance='no') "------>"
                write(INPUT_, '(F16.3)', advance='no') current_load%get_start_load()
                write(INPUT_, '(A)', advance='no') "-"
                write(INPUT_, '(F16.3)') current_load%get_end_load()
            else
                write(INPUT_, '(F16.3)', advance='no') current_load%get_start_location()
                write(INPUT_, '(A8)', advance='no') "------>"
                write(INPUT_, '(F16.3)') current_load%get_start_load()
            end if
        end do
        write(INPUT_, '(A)') ""


        ! supports
        write(INPUT_, '(A)') "Support: "
        write(INPUT_, '(A1)', advance='no') "("
        write(INPUT_, '(A12)', advance='no') "Tipe Support"
        write(INPUT_, '(A2)', advance='no') ") "
        write(INPUT_, '(A16)') "Lokasi"
        do i = 1, supports%get_size()
            current_sup = supports%get_support_by_index(i)

            write(INPUT_, '(A1)', advance='no') "("
            write(INPUT_, '(A12)', advance='no') get_type_support(current_sup%get_type())
            write(INPUT_, '(A2)', advance='no') ") "

            write(INPUT_, '(F16.3)') current_sup%get_location()
        end do
        write(INPUT_, '(A)') ""

        ! Close the file
        close(INPUT_)

    end subroutine write_input

    subroutine write_output(result_)
        type(Result), intent(inout) :: result_
        type(ResultLoadArrayList) :: reactions
        type(ResultLoadArrayList) :: moment_reactions
        type(ResultLoad) :: current_result
        type(ResultLoadArrayList), allocatable :: moments(:)
        type(ResultLoadArrayList), allocatable :: sheers(:)
        type(ResultLoadArrayList) :: current_result_list
        type(LocationIntervalArrayList) :: intervals
        type(LocationInterval) :: current_inter
        character(len=100) :: filename
        integer :: i, j

        filename = './output/output.txt'
        reactions = result_%get_reactions()
        moment_reactions = result_%get_moment_reactions()
        moments = result_%get_moments()
        sheers = result_%get_sheers()
        intervals = get_intervals()

        call create_output_directory()
        open(unit=OUTPUT_, file=filename, status='unknown')

        !reaksi
        write(OUTPUT_, '(A)') "Reaksi: "
        write(OUTPUT_, '(A16)', advance='no') "Lokasi"
        write(OUTPUT_, '(A8)', advance='no') "------>"
        write(OUTPUT_, '(A16)') "Besar Reaksi"
        do i = 1, reactions%get_size()
            current_result = reactions%get_resultload(i)
            write(OUTPUT_, '(F16.3)', advance='no') current_result%get_location()
            write(OUTPUT_, '(A8)', advance='no') "------>"
            write(OUTPUT_, '(F16.3)') current_result%get_load()
        end do
        write(OUTPUT_, '(A)') ""

        !momen reaksi
        if(moment_reactions%get_size() > 0) then
            write(OUTPUT_, '(A)') "Momen: "
            write(OUTPUT_, '(A16)', advance='no') "Lokasi"
            write(OUTPUT_, '(A8)', advance='no') "------>"
            write(OUTPUT_, '(A16)') "Besar Momen"
            do i = 1, moment_reactions%get_size()
                current_result = moment_reactions%get_resultload(i)
                write(OUTPUT_, '(F16.3)', advance='no') current_result%get_location()
                write(OUTPUT_, '(A8)', advance='no') "------>"
                write(OUTPUT_, '(F16.3)') current_result%get_load()
            end do
            write(OUTPUT_, '(A)') ""
        end if

        !Bidang D
        write(OUTPUT_, '(A)') ""
        write(OUTPUT_, '(A)') "==========================================="
        write(OUTPUT_, '(A)') "Bidang D"
        write(OUTPUT_, '(A)') "==========================================="
        do i = 1, intervals%get_size()
            current_inter = intervals%get_location_lnterval(i)
            current_result_list = sheers(i)

            write(OUTPUT_, '(A)') ""
            write(OUTPUT_, '(A8)', advance='NO') "Interval"
            write(OUTPUT_, '(F8.2)', advance='NO') current_inter%get_start()
            write(OUTPUT_, '(A16)', advance='NO') " <= x <= "
            write(OUTPUT_, '(F16.2)', advance='NO') current_inter%get_end()
            write(OUTPUT_, '(A)') ""
            write(OUTPUT_, '(A16)', advance='NO') "Lokasi"
            write(OUTPUT_, '(A16)', advance='NO') " --> "
            write(OUTPUT_, '(A16)') "Gaya"
            write(OUTPUT_, '(A)') ""
            do j = 1, current_result_list%get_size()
                current_result = current_result_list%get_resultload(j)
                write(OUTPUT_, '(F16.2)', ADVANCE='NO') current_result%get_location()
                write(OUTPUT_, '(A16)', ADVANCE='NO') " --> "
                write(OUTPUT_, '(F16.2)') current_result%get_load()
            end do
        end do
        write(OUTPUT_, '(A)') ""

        !Bidang M
        write(OUTPUT_, '(A)') ""
        write(OUTPUT_, '(A)') "==========================================="
        write(OUTPUT_, '(A)') "Bidang M"
        write(OUTPUT_, '(A)') "==========================================="
        do i = 1, intervals%get_size()
            current_inter = intervals%get_location_lnterval(i)
            current_result_list = moments(i)

            write(OUTPUT_, '(A)') ""
            write(OUTPUT_, '(A8)', advance='NO') "Interval"
            write(OUTPUT_, '(F8.2)', advance='NO') current_inter%get_start()
            write(OUTPUT_, '(A16)', advance='NO') " <= x <= "
            write(OUTPUT_, '(F16.2)', advance='NO') current_inter%get_end()
            write(OUTPUT_, '(A)') ""
            write(OUTPUT_, '(A16)', advance='NO') "Lokasi"
            write(OUTPUT_, '(A16)', advance='NO') " --> "
            write(OUTPUT_, '(A16)') "Momen"
            write(OUTPUT_, '(A)') ""
            do j = 1, current_result_list%get_size()
                current_result = current_result_list%get_resultload(j)
                write(OUTPUT_, '(F16.2)', ADVANCE='NO') current_result%get_location()
                write(OUTPUT_, '(A16)', ADVANCE='NO') " --> "
                write(OUTPUT_, '(F16.2)') current_result%get_load()
            end do
        end do
        write(OUTPUT_, '(A)') ""

        ! Close the file
        close(OUTPUT_)

    end subroutine write_output

    subroutine create_output_directory()
        logical :: directory_exists
        integer :: ierr

        ! Check if the directory exists
        inquire(file=trim("output"), exist=directory_exists)

        if (.not.directory_exists) then
            call system("mkdir output")
        end if
    end subroutine create_output_directory

    function get_type_load(type_) result(type_load)
        integer, intent(in) :: type_
        character(len=100) :: type_load

        select case (type_)
            case (POINT_)
                type_load = "Point"
            case (MOMENT_)
                type_load = "Moment"
            case (DISTRIBUTED_)
                type_load = "Distributed"
        end select

    end function get_type_load

    function get_type_support(type_) result(type_sup)
        integer, intent(in) :: type_
        character(len=100) :: type_sup

        select case (type_)
            case (PIN_)
                type_sup = "Pin"
            case (ROLLER_)
                type_sup = "Roller"
            case (FIXED_)
                type_sup = "Fixed"
        end select

    end function get_type_support

end module io_service
