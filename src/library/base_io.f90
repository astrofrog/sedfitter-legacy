module base_io

  use base_types
  use base_messages
  use base_string
  use iso_fortran_env, only : iostat_end
  use posix
  
  implicit none
  save

  private
  public :: list_files
  public :: delete_dir,delete_file
  public :: dir_exists,file_exists
  public :: check_file_exists,check_dir_exists
  public :: len_ascii,len_bin,file_n_lines,file_n_header_lines
  public :: present_arg,real_arg,integer_arg,char_arg,logical_arg
  public :: read_column
  public :: next_unit,open_safe,open_status

  logical,private :: debug = .false.

  interface read_column
     module procedure read_column_real8
     module procedure read_column_real4
     module procedure read_column_integer
     module procedure read_column_char
  end interface

contains
  
  subroutine list_files(directory,files,pattern)
    implicit none
    character(len=*),intent(in)  :: directory,pattern
    character(len=*),allocatable,intent(out) :: files(:)
    integer :: unit,n_files,f
    character(len=1000) :: buffer
    if(allocated(files)) deallocate(files)
    call system('find '//trim(directory)//'  -name "'//trim(pattern)//'" | sort > /tmp/list.temp')
    n_files = file_n_lines('/tmp/list.temp')
    allocate(files(n_files))
    call open_safe(unit,file='/tmp/list.temp',status='old')
    do f=1,n_files
      read(unit,'(A1000)') buffer
      files(f)=adjustl(trim(buffer))
    end do
    close(unit)
    call system('rm /tmp/list.temp')
  end subroutine list_files
  
  !**********************************************************************
  ! Delete directory (with prompt) and make new empty directory
  !**********************************************************************

  subroutine delete_dir(dir)

    implicit none

    character(len=*),intent(in) :: dir
    ! the directory to look for

    character(len=3) :: rep
    ! user reply (YES/NO)

    character(len=100) :: command,pretty_command
    ! command to delete the directory

    character(len=3) :: len_char
    ! length of string

    if(dir_exists(dir)) then

       command="rm -r "//trim(dir)
       pretty_command="["//trim(command)//"]"

       write(len_char,'(I3.3)') len_trim(dir)
       write(*,'(" WARNING: Directory exists: ",A'//len_char//')') dir

       write(len_char,'(I3.3)') len_trim(pretty_command)     
       write(*,'(" The following command will be run: ",A'//len_char//')') pretty_command

       write(*,'(" Do you wish to continue? (y/n) ")',advance='no')

       do 
          read *,rep
          if(rep=="y".or.rep=="n") exit
          print *,'Please type y or n (case sensitive)'
       end do
       if(rep=='n') then
          print *,'Aborting to avoid overwriting files'
          stop
       end if
       call system(command)
    end if

    print *,' -> Creating directory: ',trim(dir)
    call system('mkdir '//dir)

  end subroutine delete_dir

  !**********************************************************************
  ! Delete file (with prompt)
  !**********************************************************************

  subroutine delete_file(file)

    implicit none

    character(len=*),intent(in) :: file
    ! the fileectory to look for

    character(len=3) :: rep
    ! user reply (YES/NO)

    character(len=100) :: command,pretty_command
    ! command to delete the file

    character(len=3) :: len_char
    ! length of the character to display

    if(file_exists(file)) then

       command="rm "//trim(file)
       pretty_command="["//trim(command)//"]"

       write(len_char,'(I3.3)') len_trim(file)
       write(*,'(" WARNING: File exists: ",A'//len_char//')') file

       write(len_char,'(I3.3)') len_trim(pretty_command)     
       write(*,'(" The following command will be run: ",A'//len_char//')') pretty_command

       write(*,'(" Do you wish to continue? (y/n) ")',advance='no')

       do 
          read *,rep
          if(rep=="y".or.rep=="n") exit
          print *,'Please type y or n (case sensitive)'
       end do
       if(rep=='n') then
          print *,'Aborting to avoid overwriting file'
          stop
       end if

       call system(command)

    end if

    open (29,file=file, form='unformatted')
    close (29,status='delete')
!    call flush(29)

  end subroutine delete_file

  !**********************************************************************
  ! Check that file exists and produce error if not
  !**********************************************************************
  
  subroutine check_file_exists(file)

    use base_messages
    implicit none

    character(len=*),intent(in) :: file
    ! the file to look for

    if(.not.file_exists(file)) then
       call error("check_file_exists","File does not exist : "//trim(file))
    end if

  end subroutine check_file_exists

  !**********************************************************************
  ! Check that directory exists and produce error if not
  !********************************************************************** 

  subroutine check_dir_exists(dir)

    use base_messages
    implicit none

    character(len=*),intent(in) :: dir
    ! the directory to look for

    if(.not.file_exists(dir)) then
       call error("check_dir_exists","Directory does not exist : "//trim(dir))
    end if

  end subroutine check_dir_exists

  !**********************************************************************
  ! Find length of unformatted file
  !**********************************************************************

  integer function len_bin(filename,header)

    implicit none

    character(len=*),intent(in) :: filename
    ! the binary file to find the length of

    logical,intent(in) :: header

    integer :: count
    ! how many lines have been read so far

    integer :: ioerr
    ! used for I/O errors

    count=0

    open(unit=10,file=filename,form='unformatted',status='old',iostat=ioerr)
    call open_status(ioerr,filename)

    if(header) read(10)
    do
       read(10,iostat=ioerr)
       if(ioerr.ne.0) exit
       count=count+1
    end do
    close(10)

    len_bin=count

  end function len_bin

  !**********************************************************************
  ! Find length of ASCII file
  !**********************************************************************

  integer function len_ascii(filename,header) ! V2C

    implicit none

    character(len=*),intent(in) :: filename
    ! the ascii file to find the length of

    logical,intent(in) :: header

    integer :: count
    ! how many lines have been read so far

    integer :: ioerr
    ! used for I/O errors

    count=0

    open(unit=10,file=filename,status='old',iostat=ioerr)
    call open_status(ioerr,filename)

    if(header) read(10,*)
    do
       read(10,*,iostat=ioerr)
       if(ioerr.ne.0) exit
       count=count+1
    end do
    close(10)

    len_ascii=count

  end function len_ascii

  !**********************************************************************
  ! Find length of ASCII file (new)
  !**********************************************************************

  integer function file_n_lines(filename,unformatted)

    implicit none

    character(len=*),intent(in) :: filename
    ! the ascii file to find the length of

    integer :: count
    ! how many lines have been read so far

    integer :: ioerr
    ! used for I/O errors

    integer :: u

    logical,optional :: unformatted
    logical :: formatted
    
    formatted = .true.

    if(present(unformatted)) then
       formatted = .not.unformatted
    end if
    
    count=0

    call open_safe(u,filename,status='old')

    do

       if(formatted) then
          read(u,*,iostat=ioerr)
       else
          read(u,iostat=ioerr)
       end if

       if(ioerr.ne.0) exit
       count=count+1

    end do

    close(u)

    file_n_lines=count

  end function file_n_lines


  !**********************************************************************
  ! Find length of header (comments) section in ASCII file
  !**********************************************************************

  integer function file_n_header_lines(filename)

    use base_messages

    implicit none

    character(len=*),intent(in) :: filename
    ! the ascii file to find the length of

    integer :: count
    ! how many lines have been read so far

    integer :: ioerr
    ! used for I/O errors

    integer :: u

    character(len=1) :: first

    character(len=1),parameter :: slash = achar(92)

    character(len=1000) :: line

    count=0

    call open_safe(u,filename,status='old')

    do
       read(u,'(A1000)',iostat=ioerr) line
       line = adjustl(line)
       first = line(1:1)
       if(ioerr.ne.0) exit
       select case(first)
       case('#',slash,'|')
          count = count + 1
       case default
          exit
       end select
    end do

    close(u)

    file_n_header_lines=count

  end function file_n_header_lines

  !**********************************************************************
  ! Display open status message
  !**********************************************************************

  subroutine open_status(ioerr,filename)

    use base_messages, only : error
    implicit none

    integer,intent(in) :: ioerr

    character(len=*),intent(in) :: filename

    if(ioerr.ne.0) call error("open_status","File not found : "//trim(filename))

  end subroutine open_status

  !**********************************************************************
  ! Open file (with error checking)
  !**********************************************************************

  subroutine open_safe(unit,file,status,form,position)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: file
    ! the file to open

    character(len=*),intent(in),optional :: status,form,position
    ! whether the file exists already, whether to write it in ascii form,
    ! and whether to write in append mode

    ! --- Output --- !

    integer,intent(out) :: unit
    ! the unit that is used

    character(len=15) :: opt_status,opt_form,opt_position

    ! Find free unit

    unit = next_unit()

    ! Set default options
    
    opt_status = 'replace'
    opt_form   = 'formatted'
    opt_position = 'asis'

    if(present(status)) opt_status = status
    if(present(form))   opt_form   = form
    if(present(position)) opt_position = position

    if(opt_position=='append') opt_status='old'

    if(opt_status=='old') then
       call check_file_exists(file)
    else
       call delete_file(file)
    end if

    open(unit=unit,file=file,status=opt_status,form=opt_form,position=opt_position)

  end subroutine open_safe

  !**********************************************************************
  ! Find next free I/O unit
  !**********************************************************************

  integer function next_unit()

    implicit none

    integer,parameter :: u_start = 10
    ! the unit number to start searching at

    integer :: u
    ! loop variable

    logical :: op
    ! whether the unit is already in use

    do u=u_start,1000
       inquire(unit=u,opened=op)
       if(.not.op) then
          next_unit=u
          exit
       end if
    end do

    return

  end function next_unit

  !**********************************************************************
  ! Find whether a given argument is present on the command-line
  !**********************************************************************

  logical function present_arg(name)

    ! This function finds out whether a given argument is present
    ! in the command.

    implicit none

    character(len=*),intent(in) :: name
    ! the character to search for

    character(len=100) :: argument
    ! the command-line argument

    character(len=100) :: sub1,sub2
    ! the two parts of the command-line argument

    integer :: i,p
    ! loop and position variable

    present_arg=.false.

    do i=1,100

      call get_command_argument(i,argument)
      !call getarg(i,argument)

       p=index(argument,'=')

       sub1=trim(adjustl(argument(1:p-1)))
       sub2=trim(adjustl(argument(p+1:100)))

       if(trim(sub1)==trim(name)) then

          present_arg=.true.

          return

       end if

    end do

    return

  end function present_arg


  !**********************************************************************
  ! Retrieve a character string from command-line arguments
  !**********************************************************************

  logical function logical_arg(name)

    ! This function cycles through the command-line arguments,
    ! looking for name=value, and returns value as a character.

    implicit none

    character(len=*),intent(in) :: name
    ! the character to search for

    character(len=1000) :: argument
    ! the command-line argument

    character(len=1000) :: sub1,sub2
    ! the two parts of the command-line argument

    integer :: i,p
    ! loop and position variable

    do i=1,1000

      call get_command_argument(i,argument)
        !call getarg(i,argument)

       p=index(argument,'=')

       sub1=trim(adjustl(argument(1:p-1)))
       sub2=trim(adjustl(argument(p+1:1000)))

       if(trim(sub1)==trim(name)) then

          call read_from_string(sub2,logical_arg)

          return

       end if

    end do

    print *,'ERROR: option not found: ',trim(name)
    print *,''

    stop

  end function logical_arg


  !**********************************************************************
  ! Retrieve a character string from command-line arguments
  !**********************************************************************

  real(dp) function real8_arg(name)

    ! This function cycles through the command-line arguments,
    ! looking for name=value, and returns value as a character.

    implicit none

    character(len=*),intent(in) :: name
    ! the character to search for

    character(len=1000) :: argument
    ! the command-line argument

    character(len=1000) :: sub1,sub2
    ! the two parts of the command-line argument

    integer :: i,p
    ! loop and position variable

    do i=1,1000

      call get_command_argument(i,argument)
        !call getarg(i,argument)

       p=index(argument,'=')

       sub1=trim(adjustl(argument(1:p-1)))
       sub2=trim(adjustl(argument(p+1:1000)))

       if(trim(sub1)==trim(name)) then

          call read_from_string(sub2,real8_arg)

          return

       end if

    end do

    print *,'ERROR: option not found: ',trim(name)
    print *,''

    stop

  end function real8_arg


  !**********************************************************************
  ! Retrieve a character string from command-line arguments
  !**********************************************************************

  real(sp) function real_arg(name)

    ! This function cycles through the command-line arguments,
    ! looking for name=value, and returns value as a character.

    implicit none

    character(len=*),intent(in) :: name
    ! the character to search for

    character(len=1000) :: argument
    ! the command-line argument

    character(len=1000) :: sub1,sub2
    ! the two parts of the command-line argument

    integer :: i,p
    ! loop and position variable

    do i=1,1000

      call get_command_argument(i,argument)
        !call getarg(i,argument)

       p=index(argument,'=')

       sub1=trim(adjustl(argument(1:p-1)))
       sub2=trim(adjustl(argument(p+1:1000)))

       if(trim(sub1)==trim(name)) then

          call read_from_string(sub2,real_arg)

          return

       end if

    end do

    print *,'ERROR: option not found: ',trim(name)
    print *,''

    stop

  end function real_arg


  !**********************************************************************
  ! Retrieve a character string from command-line arguments
  !**********************************************************************

  integer function integer_arg(name)

    ! This function cycles through the command-line arguments,
    ! looking for name=value, and returns value as a character.

    implicit none

    character(len=*),intent(in) :: name
    ! the character to search for

    character(len=1000) :: argument
    ! the command-line argument

    character(len=1000) :: sub1,sub2
    ! the two parts of the command-line argument

    integer :: i,p
    ! loop and position variable

    do i=1,1000

      call get_command_argument(i,argument)
        !call getarg(i,argument)

       p=index(argument,'=')

       sub1=trim(adjustl(argument(1:p-1)))
       sub2=trim(adjustl(argument(p+1:1000)))

       if(trim(sub1)==trim(name)) then

          call read_from_string(sub2,integer_arg)

          return

       end if

    end do

    print *,'ERROR: option not found: ',trim(name)
    print *,''

    stop

  end function integer_arg


  !**********************************************************************
  ! Retrieve a character string from command-line arguments
  !**********************************************************************

  character(len=1000) function char_arg(name)

    ! This function cycles through the command-line arguments,
    ! looking for name=value, and returns value as a character.

    implicit none

    character(len=*),intent(in) :: name
    ! the character to search for

    character(len=1000) :: argument
    ! the command-line argument

    character(len=1000) :: sub1,sub2
    ! the two parts of the command-line argument

    integer :: i,p
    ! loop and position variable

    do i=1,1000

      call get_command_argument(i,argument)
        !call getarg(i,argument)

       p=index(argument,'=')

       sub1=trim(adjustl(argument(1:p-1)))
       sub2=trim(adjustl(argument(p+1:1000)))

       if(trim(sub1)==trim(name)) then

          call read_from_string(sub2,char_arg)

          return

       end if

    end do

    print *,'ERROR: option not found: ',trim(name)
    print *,''

    stop

  end function char_arg


  !**********************************************************************
  ! Read column from ASCII file
  !**********************************************************************


  subroutine read_column_real8(filename,column,values)

    implicit none

    character(len=*),intent(in) :: filename
    ! the name of the file to read the columns from

    integer,intent(in) :: column
    ! the column number

    real(dp),allocatable :: values(:)
    ! the values

    integer :: i,j,u
    ! loop variable

    integer :: n_header,n_total,n_read
    ! the number of lines in the file

    character(len=100) :: dum
    ! dummy variable

    integer :: ioerr
    ! I/O error status

    if(allocated(values)) deallocate(values)

    n_header = file_n_header_lines(filename)
    n_total  = file_n_lines(filename)
    n_read   = n_total - n_header

    allocate(values(n_read))

    call open_safe(u,filename,status='old')

    if(debug) write(*,'("Skipping ",I6," header lines")') n_header
    do i=1,n_header
       read(u,*)
    end do

    if(debug) write(*,'("Reading  ",I6," data lines")') n_read
    do i=1,n_read
       read(u,*,iostat=ioerr) (dum,j=1,column-1),values(i)
       if(ioerr==iostat_end) then
         call error("read_column_real8","end of file reached in "//trim(filename))
       end if
    end do

    close(unit=u)

  end subroutine read_column_real8


  subroutine read_column_real4(filename,column,values)

    implicit none

    character(len=*),intent(in) :: filename
    ! the name of the file to read the columns from

    integer,intent(in) :: column
    ! the column number

    real(sp),allocatable :: values(:)
    ! the values

    integer :: i,j,u
    ! loop variable

    integer :: n_header,n_total,n_read
    ! the number of lines in the file

    character(len=100) :: dum
    ! dummy variable

    integer :: ioerr
    ! I/O error status

    if(allocated(values)) deallocate(values)

    n_header = file_n_header_lines(filename)
    n_total  = file_n_lines(filename)
    n_read   = n_total - n_header

    allocate(values(n_read))

    call open_safe(u,filename,status='old')

    if(debug) write(*,'("Skipping ",I6," header lines")') n_header
    do i=1,n_header
       read(u,*)
    end do

    if(debug) write(*,'("Reading  ",I6," data lines")') n_read
    do i=1,n_read
       read(u,*,iostat=ioerr) (dum,j=1,column-1),values(i)
       if(ioerr==iostat_end) then
         call error("read_column_real4","end of file reached in "//trim(filename))
       end if
    end do

    close(unit=u)

  end subroutine read_column_real4


  subroutine read_column_integer(filename,column,values)

    implicit none

    character(len=*),intent(in) :: filename
    ! the name of the file to read the columns from

    integer,intent(in) :: column
    ! the column number

    integer,allocatable :: values(:)
    ! the values

    integer :: i,j,u
    ! loop variable

    integer :: n_header,n_total,n_read
    ! the number of lines in the file

    character(len=100) :: dum
    ! dummy variable

    integer :: ioerr
    ! I/O error status

    if(allocated(values)) deallocate(values)

    n_header = file_n_header_lines(filename)
    n_total  = file_n_lines(filename)
    n_read   = n_total - n_header

    allocate(values(n_read))

    call open_safe(u,filename,status='old')

    if(debug) write(*,'("Skipping ",I6," header lines")') n_header
    do i=1,n_header
       read(u,*)
    end do

    if(debug) write(*,'("Reading  ",I6," data lines")') n_read
    do i=1,n_read
       read(u,*,iostat=ioerr) (dum,j=1,column-1),values(i)
       if(ioerr==iostat_end) then
         call error("read_column_integer","end of file reached in "//trim(filename))
       end if
    end do

    close(unit=u)

  end subroutine read_column_integer


  subroutine read_column_char(filename,column,values)

    implicit none

    character(len=*),intent(in) :: filename
    ! the name of the file to read the columns from

    integer,intent(in) :: column
    ! the column number

    character(len=*),allocatable :: values(:)
    ! the values

    integer :: i,j,u
    ! loop variable

    integer :: n_header,n_total,n_read
    ! the number of lines in the file

    character(len=100) :: dum
    ! dummy variable

    integer :: ioerr
    ! I/O error status

    if(allocated(values)) deallocate(values)

    n_header = file_n_header_lines(filename)
    n_total  = file_n_lines(filename)
    n_read   = n_total - n_header

    allocate(values(n_read))

    call open_safe(u,filename,status='old')

    if(debug) write(*,'("Skipping ",I6," header lines")') n_header
    do i=1,n_header
       read(u,*)
    end do

    if(debug) write(*,'("Reading  ",I6," data lines")') n_read
    do i=1,n_read
       read(u,*,iostat=ioerr) (dum,j=1,column-1),values(i)
       if(ioerr==iostat_end) then
         call error("read_column_char","end of file reached in "//trim(filename))
       end if
    end do

    close(unit=u)

  end subroutine read_column_char


  !**********************************************************************
  ! The following subroutine tries to find a unique solution to a filename
  ! containing wildcards. This means that for example if the user calls the
  ! subroutine with 'hello*.txt', the subroutine will try to find a unique
  ! match for this filename, and will cause an error if more than one file
  ! is found
  !**********************************************************************

  subroutine solve_wildcard(filename)

    implicit none

    character(len=*),intent(inout) :: filename
    character(len=500) :: filename_temp 
    ! the filename to solve

    integer :: u
    ! unit number for temporary file

    integer :: r,n_results
    ! number of possibilities and length of ascii file

    integer :: ioerr
    ! used for I/O errors

    call system('ls '//trim(filename)//' > /tmp/wildcard.tmp')

    u=next_unit()

    n_results=len_ascii('/tmp/wildcard.tmp',.false.)

    if(n_results==0) then

       write(*,*)
       write(*,*) "ERROR - no match found for: ",trim(filename)
       write(*,*)
       stop

    else if(n_results==1) then

       open(unit=u,file='/tmp/wildcard.tmp',status='old',iostat=ioerr)
       call open_status(ioerr,'/tmp/wildcard.tmp')
       read(u,'(A500)') filename_temp
       close(unit=u)

       !     write(*,*) trim(filename)//' => '//trim(filename_temp)
       !     write(*,*)

    else

       write(*,*)
       write(*,*) "ERROR - more than one possibility for: ",trim(filename)

       open(unit=u,file='/tmp/wildcard.tmp',status='old',iostat=ioerr)
       call open_status(ioerr,'/tmp/wildcard.tmp')
       do r=1,n_results
          read(u,*) filename_temp
          write(*,*) " -> "//trim(filename_temp)
       end do
       close(unit=u)

       write(*,*)
       stop

    end if

    filename=trim(filename_temp)

  end subroutine solve_wildcard

end module base_io
