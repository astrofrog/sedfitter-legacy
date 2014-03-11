!**********************************************************************!
! This module contains I/O subroutines and functions
!
! Versions:
!
! Original code : TR
!
!**********************************************************************!

module base_messages

  use base_types

  implicit none
  save
  
  !########################!
  private
  public :: message_section
  public :: error
  public :: warning
  public :: delimit
  public :: message
  public :: message_number
  public :: set_verbose_level
  !########################!

  integer :: verbose_level = 0
  
  interface message_number
     module procedure message_number_dp,message_number_int
  end interface

contains
  
  subroutine set_verbose_level(level)
    implicit none
    integer,intent(in) :: level
    verbose_level = level
  end subroutine set_verbose_level
  
  subroutine message(level,text)
    implicit none
    integer,intent(in)          :: level
    character(len=*),intent(in) :: text
    if(level <= verbose_level) write(*,*) text
  end subroutine message
  
  subroutine message_number_dp(level,text,number,format,units)
    implicit none
    integer,intent(in)          :: level
    character(len=*),intent(in) :: text
    real(dp),intent(in)         :: number
    character(len=*),intent(in) :: format,units
    character(len=20)           :: char_number
    write(char_number,format) number
    if(level <= verbose_level) write(*,*) trim(text)//' '//trim(adjustl(char_number))//' '//trim(units)
  end subroutine message_number_dp
  
  subroutine message_number_int(level,text,number,format,units)
    implicit none
    integer,intent(in)          :: level
    character(len=*),intent(in) :: text
    integer,intent(in)          :: number
    character(len=*),intent(in) :: format,units
    character(len=20)           :: char_number
    write(char_number,format) number
    if(level <= verbose_level) write(*,*) trim(text)//' '//trim(adjustl(char_number))//' '//trim(units)
  end subroutine message_number_int

  subroutine message_section(text)
    implicit none
    character(len=*),intent(in) :: text
    write(*,*)
    call delimit
    write(*,*) ' => '//trim(text)
    call delimit
    write(*,*)
  end subroutine message_section


  !**********************************************************************!
  ! This subroutine is used to display warnings
  !
  ! Versions: 
  !
  ! Original code : BAW - initial subroutine
  ! April 2007    : TR  - conversion to f90
  !**********************************************************************!

  subroutine warning(location,text)
    implicit none
    character(len=*),intent(in) :: location,text
    call delimit
    write(*,*) "WARNING : ",trim(text)
    write(*,*) "WHERE   : ",trim(location)
    call delimit
  end subroutine warning

  !**********************************************************************!
  ! This subroutine is used to display errors and stop execution
  !
  ! Versions: 
  !
  ! Original code : BAW - initial subroutine
  ! April 2007    : TR  - conversion to f90
  !**********************************************************************!

  subroutine error(location,text)

    implicit none

    character(len=*),intent(in) :: location,text

    character(len=8) :: date
    character(len=10) :: time

    integer :: m

    call date_and_time(date,time)
    read(date(5:6),*) m

    call delimit
    write(*,*) "ERROR   : ",trim(text)
    write(*,*) "WHERE   : ",trim(location)
    call delimit

    write(*,*)
    write(*,*) " *** Execution aborted on "&
         &//date(7:8)//" "//trim(month(m))//" "//trim(date(1:4))//" at "&
         &//time(1:2)//":"//time(3:4)//":"//time(5:6)//" ***"
    write(*,*) 

    stop

  end subroutine error

  !**********************************************************************!
  ! This subroutine is used to return the name of a month given the
  ! number of the month. 
  !
  ! Versions: 
  !
  ! Original code : TR
  !**********************************************************************!

  character(len=20) function month(i)

    implicit none

    integer,intent(in) :: i

    if(i==1) month="January"
    if(i==2) month="February"
    if(i==3) month="March"
    if(i==4) month="April"
    if(i==5) month="May"
    if(i==6) month="June"
    if(i==7) month="July"
    if(i==8) month="August"
    if(i==9) month="September"
    if(i==10) month="October"
    if(i==11) month="November"
    if(i==12) month="December"

  end function month

  !**********************************************************************!
  ! This subroutine is used to display a delimiter
  !
  ! Versions:
  !
  ! Original code : TR
  !**********************************************************************!

  subroutine delimit
    implicit none
    write(*,*) repeat('-',60)
  end subroutine delimit

end module base_messages

!**********************************************************************!
