# 1 "D:/Programming/Fortran/StructureAnalysis/services/introduction_module.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "D:\\Programming\\Fortran\\StructureAnalysis\\cmake-build-debug//"
# 1 "D:/Programming/Fortran/StructureAnalysis/services/introduction_module.f90"
! Created by hamiz on 12/1/2024.
module message_module
    implicit none

    public :: display_intro

    contains

    subroutine display_intro()
        print *, "======================================================="
        print *, " Program Perhitungan Reaksi Perletakan dan Perhitungan"
        print *, "     Bidang D dan M pada Struktur Statis Tertentu  "
        print *, "======================================================="
        Print *, "Tujuan Program:"
        Print *, "Program ini dibuat untuk mempermudah dalam menghitung reaksi perletakan"
        Print *, "dan perhitungan Bidang D dan M pada Struktur Statis tertentu"
        print *,""
        print *, "Dikerjakan oleh:"
        Print *, "1.Muhammad Madani Hamizul Fuad (235060100111030)"
        Print *, "2.Putu Sri Adhi Darsana (235060100111032)"
        Print *, "3.Athar Ravadhan Madjid (235060107111018)"
        Print *, "4.Reza Daniel (235060100111028)"
        Print *, "======================================================="
        Print *, ""
    end subroutine display_intro




end module message_module
