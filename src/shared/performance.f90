!#############################################################################
! Purpose : display performance statistics
! Author  : Thomas Robitaille
! Date    : 8th July 2006
! Checked : Yes
! Known Issues: None
! Changes:  17 July 2006 - fixed problem with very low performance causing
!                          division by zero
!#############################################################################

module performance

  use base_types
  
  implicit none
  save

  private
  public :: cpu_init
  public :: cpu_display
  public :: cpu_display_last

  real(sp) :: sample_time_1,sample_time_2
  ! start and end time for the first source

  real(sp) :: time_1,time_2
  ! time interval to compute the statistics

  integer :: display_num
  ! how often to display statistics

contains

!#############################################################################

subroutine cpu_init
  implicit none
  call cpu_time(sample_time_1)
  time_1=sample_time_1
  write(*,'("   # Sources    CPU time (sec)    Sources/sec  ")')
  write(*,'(" ----------------------------------------------")')
end subroutine cpu_init

!#############################################################################

subroutine cpu_sample_end
  implicit none
  real(sp) :: num
  call cpu_time(sample_time_2)
  num=log10(10./(sample_time_2-sample_time_1+1.e-5))
  if(num.lt.0) num=0.
  display_num=10**nint(num)
end subroutine cpu_sample_end

!#############################################################################

subroutine cpu_display(count)
  implicit none
  integer,intent(in) :: count
  ! how many sources have been processed
  if(count==1) call cpu_sample_end
  if(mod(count,display_num)==0) then
     call cpu_time(time_2)
     write(*,'(1X,3X,I7,3X,4X,F10.1,4X,4X,F7.2,4X)') count,time_2-time_1,real(count)/(time_2-time_1+1.e-30)
  end if
end subroutine cpu_display

subroutine cpu_display_last(count)
  implicit none
  integer,intent(in) :: count
  ! how many sources have been processed
  if(mod(count,display_num).ne.0) then
     call cpu_time(time_2)
     write(*,'(1X,3X,I7,3X,4X,F10.1,4X,4X,F7.2,4X)') count,time_2-time_1,real(count)/(time_2-time_1+1.e-30)
  end if
end subroutine cpu_display_last
 
!#############################################################################
end module performance
