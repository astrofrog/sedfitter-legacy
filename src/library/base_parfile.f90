!**********************************************************************!
! This module contains subroutines to make it easy to read in
! parameter files. The expected format of a .par file is
! an ASCII file with lines containing lines such as:
!
! value = parameter = comments
!
! Note that character variables should be enclosed in inverted commas
! especially when givin paths to files. Commented lines (starting with
! a #) and blank lines are allowed.
!
! The subroutines are:
!
! load_par_file(filename) - load a .par file into RAM, ignoring
!                           comments and blank lines
! read_par(parameter,value) - read in the value for 'parameter'
!
! read_par is in fact an interface to four different subroutines
! depending on the type of the 'value' variable. This variable can
! be real(sp), real(dp), integer, or character(len=*)
!
!**********************************************************************!

module base_parfile

  use base_types
  use base_io
  use base_messages
  use base_string

  implicit none
  save
  
  !########################!
  private
  public :: load_par_file
  public :: read_par
  !########################!
  
  integer,parameter,private :: par_file_len=1000
  integer,parameter,private :: par_line_len=100
  character(len=6),parameter :: par_line_fmt='(A100)'

  integer,private :: n_lines
  ! number of lines is the currently loaded par file

  character(len=100),dimension(1000),private :: par_value = ""
  character(len=100),dimension(1000),private :: par_key = ""
  ! the lines of the par file

  interface read_par
     module procedure par_real8,par_real4,par_char,par_int,par_logical
  end interface

contains

  subroutine load_par_file(filename)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: filename
    ! the name of the .par file to read in

    ! --- Local variables --- !

    character(len=par_line_len) :: line_temp

    integer :: unit
    ! file unit

    integer :: ioerr
    ! used for I/O errors

    integer :: p1,p2
    ! position of first and second equal sign

    call open_safe(unit,file=filename,status='old')

    n_lines = 0
    par_key = '' ; par_value = ''

    do

       ! --- Read in the next line --- !

       read(unit,par_line_fmt,iostat=ioerr) line_temp
       if(ioerr.ne.0) exit

       ! --- Remove preceding whitespace --- !

       line_temp = adjustl(line_temp)

       p1 = index(line_temp,'=')
       p2 = index(line_temp(p1+1:),'=') + p1

       ! --- Ignore lines with nothing and lines with comments --- !

       if(line_temp(1:1).ne.'#'.and.trim(line_temp).ne.'') then
          if(p1 > 0 .and. p2 > 0) then
             n_lines = n_lines + 1
             par_value(n_lines) = adjustl(line_temp(1:p1-1))
             par_key(n_lines) = adjustl(line_temp(p1+1:p2-1))
          end if
       end if

    end do

    close(unit)

  end subroutine load_par_file


  subroutine par_logical(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)
    
    ! --- Output --- !

    logical,intent(out) :: value
    ! the value that was read in

    character(len=100) :: par_name_new
    ! temporary parameter name (e.g. with array index)

    integer :: i
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       par_name_new = concat(par_name,element,"")
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(trim(par_name_new).eq.trim(par_key(i))) then

          call read_from_string(par_value(i),value)
          found = .true.

       end if

    end do

    if(.not.found) call error("read_par","Parameter not found : "//trim(par_name_new))

  end subroutine par_logical


  subroutine par_real8(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)
    
    ! --- Output --- !

    real(dp),intent(out) :: value
    ! the value that was read in

    character(len=100) :: par_name_new
    ! temporary parameter name (e.g. with array index)

    integer :: i
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       par_name_new = concat(par_name,element,"")
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(trim(par_name_new).eq.trim(par_key(i))) then

          call read_from_string(par_value(i),value)
          found = .true.

       end if

    end do

    if(.not.found) call error("read_par","Parameter not found : "//trim(par_name_new))

  end subroutine par_real8


  subroutine par_real4(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)
    
    ! --- Output --- !

    real(sp),intent(out) :: value
    ! the value that was read in

    character(len=100) :: par_name_new
    ! temporary parameter name (e.g. with array index)

    integer :: i
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       par_name_new = concat(par_name,element,"")
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(trim(par_name_new).eq.trim(par_key(i))) then

          call read_from_string(par_value(i),value)
          found = .true.

       end if

    end do

    if(.not.found) call error("read_par","Parameter not found : "//trim(par_name_new))

  end subroutine par_real4


  subroutine par_char(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)
    
    ! --- Output --- !

    character(len=*),intent(out) :: value
    ! the value that was read in

    character(len=100) :: par_name_new
    ! temporary parameter name (e.g. with array index)

    integer :: i
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       par_name_new = concat(par_name,element,"")
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(trim(par_name_new).eq.trim(par_key(i))) then

          call read_from_string(par_value(i),value)
          found = .true.

       end if

    end do

    if(.not.found) call error("read_par","Parameter not found : "//trim(par_name_new))

  end subroutine par_char


  subroutine par_int(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)
    
    ! --- Output --- !

    integer,intent(out) :: value
    ! the value that was read in

    character(len=100) :: par_name_new
    ! temporary parameter name (e.g. with array index)

    integer :: i
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       par_name_new = concat(par_name,element,"")
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(trim(par_name_new).eq.trim(par_key(i))) then

          call read_from_string(par_value(i),value)
          found = .true.

       end if

    end do

    if(.not.found) call error("read_par","Parameter not found : "//trim(par_name_new))

  end subroutine par_int


end module base_parfile
