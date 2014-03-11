module posix

  implicit none
  save
  
!  interface
!    subroutine system(command)
!      character(len=*),intent(in) :: command
!    end subroutine system
!    subroutine flush(unit)
!      integer,intent(in) :: unit
!    end subroutine flush
!  end interface

contains

  logical function file_exists(file)
    implicit none
    character(len=*),intent(in) :: file
    integer :: access
    file_exists = access(file,' ').eq.0
  end function file_exists

  logical function dir_exists(dir)
    implicit none
    character(len=*),intent(in) :: dir
    integer :: access
    dir_exists = access(dir,' ').eq.0
  end function dir_exists
  
end module posix