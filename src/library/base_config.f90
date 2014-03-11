!**********************************************************************!
! This module contains subroutines to make it easy to read in
! configuration files. The expected format of a .conf file is
! an ASCII file with lines containing lines such as:
!
! parameter = value
!
! Note that character variables should be enclosed in inverted commas
! especially when givin paths to files. Commented lines (starting with
! a #) and blank lines are allowed. Comments directly following a
! parameter statement on the same line:
!
! parameter = value  # this is an INVALID comment
!
! are NOT allowed
!
! The subroutines are:
!
! load_config_file(filename) - load a config file into RAM, ignoring
!                              comments and blank lines
! read_config(parameter,value) - read in the value for 'parameter'
!
! read_config is in fact an interface to four different subroutines
! depending on the type of the 'value' variable. This variable can
! be real(sp), real(dp), integer, or character(len=*)
!
!**********************************************************************!

module base_config

  use base_types
  use base_io
  use base_messages
  use base_string

  implicit none
  save
  
  !########################!
  private
  public :: load_config_file
  public :: read_config
  !########################!
  
  integer,parameter,private :: config_file_len=1000
  integer,parameter,private :: config_line_len=100
  character(len=6),parameter :: config_line_fmt='(A100)'

  integer,private :: n_lines
  ! number of lines is the currently loaded config file

  character(len=config_line_len),dimension(config_file_len),private :: line
  ! the lines of the config file

  interface read_config
     module procedure config_real8,config_real4,config_char,config_int,config_logical
  end interface

contains

  subroutine load_config_file(filename)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: filename
    ! the name of the .conf file to read in

    ! --- Local variables --- !

    character(len=config_line_len) :: line_temp

    integer :: u
    ! file unit

    integer :: ioerr
    ! used for I/O errors

    call open_safe(u,filename,status='old')

    n_lines = 0
    line = ''

    do

       ! --- Read in the next line --- !

       read(u,config_line_fmt,iostat=ioerr) line_temp
       if(ioerr.ne.0) exit

       ! --- Remove preceding whitespace --- !

       line_temp = adjustl(line_temp)

       ! --- Ignore lines with nothing and lines with comments --- !

       if(line_temp(1:1).ne.'#'.and.trim(line_temp).ne.'') then

          n_lines = n_lines + 1
          line(n_lines) = line_temp

       end if

    end do

    close(unit=u)

  end subroutine load_config_file


  subroutine config_logical(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)
    
    ! --- Output --- !

    logical,intent(out) :: value
    ! the value that was read in

    character(len=100) :: c_element,par_name_new
    ! temporary parameter name (e.g. with array index)

    integer :: i,pos
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       write(c_element,'(I0)') element
       par_name_new = trim(par_name)//'('//trim(c_element)//')'
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(index(line(i),trim(par_name_new)).gt.0) then

          pos = index(line(i),'=')
          call read_from_string(line(i)(pos+1:),value)
          found = .true.

       end if

    end do

    if(.not.found) call error("read_config","Parameter not found : "//trim(par_name_new))

  end subroutine config_logical


  subroutine config_real8(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)
    
    ! --- Output --- !

    real(dp),intent(out) :: value
    ! the value that was read in

    character(len=100) :: c_element,par_name_new
    ! temporary parameter name (e.g. with array index)

    integer :: i,pos
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       write(c_element,'(I0)') element
       par_name_new = trim(par_name)//'('//trim(c_element)//')'
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(index(line(i),trim(par_name_new)).gt.0) then

          pos = index(line(i),'=')
          call read_from_string(line(i)(pos+1:),value)
          found = .true.

       end if

    end do

    if(.not.found) call error("read_config","Parameter not found : "//trim(par_name_new))

  end subroutine config_real8


  subroutine config_real4(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)
    
    ! --- Output --- !

    real(sp),intent(out) :: value
    ! the value that was read in

    character(len=100) :: c_element,par_name_new
    ! temporary parameter name (e.g. with array index)

    integer :: i,pos
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       write(c_element,'(I0)') element
       par_name_new = trim(par_name)//'('//trim(c_element)//')'
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(index(line(i),trim(par_name_new)).gt.0) then

          pos = index(line(i),'=')
          call read_from_string(line(i)(pos+1:),value)
          found = .true.

       end if

    end do

    if(.not.found) call error("read_config","Parameter not found : "//trim(par_name_new))

  end subroutine config_real4


  subroutine config_char(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)
    
    ! --- Output --- !

    character(len=*),intent(out) :: value
    ! the value that was read in

    character(len=100) :: c_element,par_name_new
    ! temporary parameter name (e.g. with array index)

    integer :: i,pos
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       write(c_element,'(I0)') element
       par_name_new = trim(par_name)//'('//trim(c_element)//')'
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(index(line(i),trim(par_name_new)).gt.0) then

          pos = index(line(i),'=')
          call read_from_string(line(i)(pos+1:),value)
          found = .true.

       end if

    end do

    if(.not.found) call error("read_config","Parameter not found : "//trim(par_name_new))

  end subroutine config_char


  subroutine config_int(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)
    
    ! --- Output --- !

    integer,intent(out) :: value
    ! the value that was read in

    character(len=100) :: c_element,par_name_new
    ! temporary parameter name (e.g. with array index)

    integer :: i,pos
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       write(c_element,'(I0)') element
       par_name_new = trim(par_name)//'('//trim(c_element)//')'
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(index(line(i),trim(par_name_new)).gt.0) then

          pos = index(line(i),'=')
          call read_from_string(line(i)(pos+1:),value)
          found = .true.

       end if

    end do

    if(.not.found) call error("read_config","Parameter not found : "//trim(par_name_new))

  end subroutine config_int


end module base_config
