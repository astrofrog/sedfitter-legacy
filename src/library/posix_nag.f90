module posix
  use f90_unix, only : flush
  use f90_unix_file
  use f90_unix_proc, only : system
contains

  logical function file_exists(file)
    implicit none
    character(len=*),intent(in) :: file
    integer :: errno
    call access(trim(file),F_OK,errno)
    file_exists = errno == 0
  end function file_exists

  logical function dir_exists(dir)
    implicit none
    character(len=*),intent(in) :: dir
    integer :: errno
    call access(trim(dir),F_OK,errno)
    dir_exists = errno == 0
  end function dir_exists

end module posix