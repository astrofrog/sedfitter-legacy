!-----------------------------------------------------------------------------!
! Author : Thomas Robitaille
! Date   : 10 October 2007
!--------------------------------------------------------- -------------------!
! The aim of this module is to provide a very high-level interface for dealing
! with some cfitsio routines
! 
! The data types are:
!
! The available routines are:
!

!-----------------------------------------------------------------------------!

module base_cfitsio
  
  use base_types
  use base_messages
  use base_io

  implicit none
  save

  private
  public :: cfitsio_check_status

  public :: base_cfitsio_verbose_enable
  public :: base_cfitsio_verbose_disable

  ! OPEN/CLOSE
  public :: fits_open_new
  public :: fits_open_read
  public :: fits_open_write
  public :: fits_close
  public :: fits_mem_to_file

  ! KEYWORDS
  public :: fits_read_keyword
  public :: fits_write_keyword
  public :: fits_exists_keyword

  ! HDU
  public :: fits_number_hdu
  public :: fits_create_hdu
  public :: fits_move_hdu
  public :: fits_write_primary_header

  ! IMAGE
  public :: fits_read_array_line
  public :: fits_write_array_line
  public :: fits_write_2d_array
  public :: fits_read_2d_array
  public :: fits_write_3d_array
  public :: fits_read_3d_array

  ! TABLE
  public :: fits_table_write_header
  public :: fits_table_write_column
  public :: fits_table_read_column
  public :: fits_table_number_rows
  public :: fits_table_number_columns
  public :: fits_table_column_number
  public :: fits_table_column_width
  public :: fits_table_new_column
  
  integer,parameter :: nullvalj = 0
  real(sp),parameter :: nullvale = 0._sp
  real(dp),parameter :: nullvald = 0._dp
  character(len=1000),parameter :: nullvals = ""
  logical,parameter :: nullvall = .false.
  character(len=1),parameter :: nullvalb = ' '

  integer :: array0dj(1) = 0
  real(sp) :: array0de(1) = 0._sp
  real(dp) :: array0dd(1) = 0._dp
  character(len=1000) :: array0ds(1) = ' '
  
  interface fits_read_2d_array
     module procedure fits_read_2d_array_d
     module procedure fits_read_2d_array_e
     module procedure fits_read_2d_array_j
  end interface

  interface fits_write_2d_array
     module procedure fits_write_2d_array_d
     module procedure fits_write_2d_array_e
     module procedure fits_write_2d_array_j
  end interface

  interface fits_read_keyword
     module procedure fits_read_kl
     module procedure fits_read_ks
     module procedure fits_read_kd
     module procedure fits_read_ke
     module procedure fits_read_kj
  end interface

  interface fits_write_keyword
     module procedure fits_write_kl
     module procedure fits_write_ks
     module procedure fits_write_kd
     module procedure fits_write_ke
     module procedure fits_write_kj
  end interface
     

  interface fits_table_write_column
     module procedure write_table_column_0d_s
     module procedure write_table_column_1d_s
     module procedure write_table_column_2d_s   
     module procedure write_table_column_0d_d
     module procedure write_table_column_1d_d
     module procedure write_table_column_2d_d   
     module procedure write_table_column_0d_e
     module procedure write_table_column_1d_e
     module procedure write_table_column_2d_e   
     module procedure write_table_column_0d_j
     module procedure write_table_column_1d_j
     module procedure write_table_column_2d_j   
  end interface

  interface fits_table_read_column
     module procedure read_table_column_0d_s
     module procedure read_table_column_1d_s
     module procedure read_table_column_2d_s
     module procedure read_table_column_1d_name_alloc_s
     module procedure read_table_column_2d_name_alloc_s
     module procedure read_table_column_0d_d
     module procedure read_table_column_1d_d
     module procedure read_table_column_2d_d
     module procedure read_table_column_1d_name_alloc_d
     module procedure read_table_column_2d_name_alloc_d
     module procedure read_table_column_0d_e
     module procedure read_table_column_1d_e
     module procedure read_table_column_2d_e
     module procedure read_table_column_1d_name_alloc_e
     module procedure read_table_column_2d_name_alloc_e
     module procedure read_table_column_0d_j
     module procedure read_table_column_1d_j
     module procedure read_table_column_2d_j
     module procedure read_table_column_1d_name_alloc_j
     module procedure read_table_column_2d_name_alloc_j
  end interface

  interface fits_write_3d_array
     module procedure fits_write_3d_array_d
     module procedure fits_write_3d_array_e
     module procedure fits_write_3d_array_j
  end interface
  
  interface fits_read_3d_array
     module procedure fits_read_3d_array_d
     module procedure fits_read_3d_array_e
     module procedure fits_read_3d_array_j
  end interface
  
  interface fits_write_array_line
    module procedure fits_write_array_line_b
    module procedure fits_write_array_line_d
    module procedure fits_write_array_line_e
  end interface
  
  interface fits_read_array_line
    module procedure fits_read_array_line_b
    module procedure fits_read_array_line_d
    module procedure fits_read_array_line_e
  end interface
  
  logical :: verbose = .false.

contains
  
  subroutine base_cfitsio_verbose_enable
    implicit none
    verbose = .true.
  end subroutine base_cfitsio_verbose_enable

  subroutine base_cfitsio_verbose_disable
    implicit none
    verbose = .false.
  end subroutine base_cfitsio_verbose_disable

  subroutine cfitsio_check_status(unit,status,origin)
    implicit none
    integer,intent(in) :: unit,status
    character(len=*),optional,intent(in) :: origin

    if(status/=0) then

       call delimit

       if(present(origin)) then
          write(*,*) 'cfitsio returned the following error in ',trim(origin),', unit=',unit
       else
          write(*,*) 'cfitsio returned the following error in : unknown, unit=',unit
       end if
       write(*,*)

       call ftrprt('STDERR',status)
       write(*,*)
       write(*,*) ' The program has terminated unexpectadly'
       call delimit
       stop

    end if

  end subroutine cfitsio_check_status
  
  subroutine fits_mem_to_file(unit)
  
    implicit none
    
    integer,intent(in) :: unit
    integer :: unit2,status
    character(len=1000) :: filename
    
    status = 0
    
    call ftflnm(unit,filename,status)
    call cfitsio_check_status(unit,status,'fits_copy [1]')
    
    if(filename(1:6).eq.'mem://') then
      
      write(*,*) 'Transfering output from memory to file'
      
      call fits_open_new(unit2,filename(7:))
      
      call ftcpfl(unit, unit2, 1, 1, 1, status)
      call cfitsio_check_status(unit,status,'fits_copy [2]')
      
      call fits_close(unit2)
      
    end if
    
  end subroutine fits_mem_to_file

  subroutine fits_open_new(unit,filename)

    implicit none

    character(len=*),intent(in) :: filename
    ! the name of the file to create

    integer,intent(out) :: unit
    ! the unit of the file that has been created

    integer :: status,blocksize
    ! cfitsio variables

    blocksize = 0

    ! Prompt for delete if existent
    if(filename(1:6).ne.'mem://') call delete_file(filename)

    ! Get free unit
    status = 0
    call ftgiou(unit,status)
    call cfitsio_check_status(unit,status,'fits_open_new [1]')

    ! Open new file for writing
    call ftinit(unit,filename,blocksize,status)
    call cfitsio_check_status(unit,status,'fits_open_new [2]')

  end subroutine fits_open_new

  subroutine fits_open_read(unit,filename)

    implicit none

    character(len=*),intent(in) :: filename
    ! the name of the file to create

    integer,intent(out) :: unit
    ! the unit of the file that has been created

    integer :: status
    integer,parameter :: readwrite = 0
    ! cfitsio variables

    status = 0

    ! Get free unit
    call ftgiou(unit,status)
    call cfitsio_check_status(unit,status,'fits_open_read')
    
    ! Open existing file for reading
    call ftnopn(unit,filename,readwrite,status)
    call cfitsio_check_status(unit,status,'fits_open_read')

  end subroutine fits_open_read

  subroutine fits_open_write(unit,filename)

    implicit none

    character(len=*),intent(in) :: filename
    ! the name of the file to create

    integer,intent(out) :: unit
    ! the unit of the file that has been created

    integer :: status,blocksize
    integer,parameter :: readwrite = 1
    ! cfitsio variables

    status = 0

    ! Get free unit
    call ftgiou(unit,status)
    call cfitsio_check_status(unit,status,'fits_open_write')
    
    ! Open existing file for reading
    call ftopen(unit,filename,readwrite,blocksize,status)
    call cfitsio_check_status(unit,status,'fits_open_write')

  end subroutine fits_open_write

  function fits_number_hdu(unit) result(hdunum)
    implicit none
    integer,intent(in) :: unit
    integer            :: status
    integer            :: hdunum
    status = 0
    call ftthdu(unit,hdunum,status)
    call cfitsio_check_status(unit,status,'fits_create_hdu')
  end function fits_number_hdu
  
  subroutine fits_create_hdu(unit,hdu_id)
    
    implicit none

    integer,intent(in) :: unit,hdu_id
    ! the file unit and the HDU that needs creating

    integer :: status
    ! cfitsio variables

    integer :: current_hdu
    ! current HDU number

    status = 0

    call ftghdn(unit,current_hdu)
    call cfitsio_check_status(unit,status,'fits_create_hdu')

    if(verbose) write(*,'("FITS | Current HDU  : ",I0)') current_hdu
    
    if(hdu_id==current_hdu+1) then
       if(verbose) write(*,'("FITS | Creating HDU : ",I0)') hdu_id
       call ftcrhd(unit,status)
       call cfitsio_check_status(unit,status,'fits_create_hdu')
    else
       write(*,'("ERROR - need to move to HDU ",I0," first")') hdu_id-1
       stop
    end if

  end subroutine fits_create_hdu

  subroutine fits_move_hdu(unit,hdu_id)
    
    implicit none

    integer,intent(in) :: unit,hdu_id
    ! the file unit and the HDU to move to

    integer :: status,hdutype
    ! cfitsio variables

    status = 0

    call ftmahd(unit,hdu_id,hdutype,status)
    call cfitsio_check_status(unit,status,'fits_move_hdu')

  end subroutine fits_move_hdu
   
  subroutine fits_close(unit)

    implicit none

    integer,intent(in) :: unit
    ! the unit to close

    integer :: status
    ! cfitsio variables

    status = 0
    
    ! Close the file
    call ftclos(unit,status)
    call cfitsio_check_status(unit,status,'fits_close')

    ! Free the unit
    call ftfiou(unit,status)
    call cfitsio_check_status(unit,status,'fits_close')

  end subroutine fits_close

  subroutine fits_write_primary_header(unit,bitpix,naxis,naxes,extend)

    implicit none

    integer,intent(in) :: unit
    ! the unit to write a FITS primary header to
    
    integer,intent(in) :: bitpix
    ! bits per pixel
    
    integer,intent(in) :: naxis
    integer,dimension(naxis),intent(in) :: naxes
    ! number of dimensions, and each dimension

    logical,intent(in) :: extend
    ! whether any extensions are present

    logical,parameter :: simple = .true.
    integer,parameter :: pcount = 0
    integer,parameter :: gcount = 1
    integer :: status

    status = 0

    call ftphpr(unit,simple,bitpix,naxis,naxes,pcount,gcount,extend,status)
    call cfitsio_check_status(unit,status,'fits_write_primary_header')

  end subroutine fits_write_primary_header

  subroutine fits_table_write_header(unit,n_rows,n_cols,title,form,units,extname)

    implicit none

    integer,intent(in) :: unit,n_rows,n_cols
    ! the unit, number of rows, and number of columns

    character(len=*),dimension(n_cols),intent(in) :: title,form,units
    ! the title, format, and units of a column

    character(len=*) :: extname
    ! the extension name of the table

    integer,parameter :: varidat = 0
    integer :: status
    ! cfitsio variables

    status = 0

    call ftphbn(unit,n_rows,n_cols,title,form,units,extname,varidat,status)
    call cfitsio_check_status(unit,status,'fits_table_write_header')

  end subroutine fits_table_write_header
  
  integer function fits_table_column_width(unit,colnum)
    implicit none
    integer,intent(in) :: unit,colnum
    integer :: status
    integer :: naxes(2),naxis
    status = 0
    call ftgtdm(unit,colnum,2,naxis,naxes,status)
    call cfitsio_check_status(unit,status,'fits_table_column_width')
    fits_table_column_width = naxes(naxis)
  end function fits_table_column_width


  subroutine read_table_column_0d_s(unit,column,values,row)

    implicit none

    integer,intent(in) :: unit,column
    integer,parameter :: n_rows = 1
    ! the unit, column number, and expected number of rows
    
    character(len=*),intent(out) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1
    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcvs(unit,column,frow,felem,n_rows,nullvals,array0ds,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_0d_s')

    values = array0ds(1)

  end subroutine read_table_column_0d_s

  subroutine read_table_column_1d_s(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    character(len=*),intent(out),dimension(:) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1
    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcvs(unit,column,frow,felem,n_rows,nullvals,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_1d_s')

  end subroutine read_table_column_1d_s

  subroutine read_table_column_2d_s(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    character(len=*),intent(out),dimension(:,:) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1

    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcvs(unit,column,frow,felem,n_rows,nullvals,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_2d_s')

  end subroutine read_table_column_2d_s
  
  subroutine read_table_column_1d_name_alloc_s(unit,colname,values)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    character(len=*),intent(out),allocatable,dimension(:) :: values
    integer :: n_rows,colnum
    integer,parameter :: frow = 1,felem = 1
    logical :: anyf
    integer :: status
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call fits_read_keyword(unit,'NAXIS2',n_rows)
    allocate(values(n_rows))
    call ftgcvs(unit,colnum,frow,felem,n_rows,nullvals,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_1d_name_alloc_s')
  end subroutine read_table_column_1d_name_alloc_s
  
  subroutine read_table_column_2d_name_alloc_s(unit,colname,values)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    character(len=*),intent(out),allocatable,dimension(:,:) :: values
    integer :: n_rows,colnum,length
    integer,parameter :: frow = 1,felem = 1
    logical :: anyf
    integer :: status
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call fits_read_keyword(unit,'NAXIS2',n_rows)
    length = fits_table_column_width(unit,colnum)
    allocate(values(length,n_rows))
    call ftgcvs(unit,colnum,frow,felem,n_rows*length,nullvals,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_2d_name_alloc_s')
  end subroutine read_table_column_2d_name_alloc_s
  
  subroutine write_table_column_0d_s(unit,column,values,row)

    implicit none

    integer,intent(in) :: unit,column
    integer,parameter :: n_rows = 1
    ! the unit, column number, and expected number of rows
    
    character(len=*),intent(in) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpcls(unit,column,frow,felem,n_rows,[values],status)
    call cfitsio_check_status(unit,status,'write_table_column_0d_s')

  end subroutine write_table_column_0d_s
  
  subroutine write_table_column_1d_s(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    character(len=*),intent(in),dimension(:) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status
    
    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpcls(unit,column,frow,felem,n_rows,values,status)
    call cfitsio_check_status(unit,status,'write_table_column_1d_s')

  end subroutine write_table_column_1d_s
  
  subroutine write_table_column_2d_s(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    character(len=*),intent(in),dimension(:,:) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status
    
    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpcls(unit,column,frow,felem,n_rows,values,status)
    call cfitsio_check_status(unit,status,'write_table_column_2d_s')

  end subroutine write_table_column_2d_s
  

  subroutine read_table_column_0d_d(unit,column,values,row)

    implicit none

    integer,intent(in) :: unit,column
    integer,parameter :: n_rows = 1
    ! the unit, column number, and expected number of rows
    
    real(dp),intent(out) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1
    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcvd(unit,column,frow,felem,n_rows,nullvald,array0dd,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_0d_d')

    values = array0dd(1)

  end subroutine read_table_column_0d_d

  subroutine read_table_column_1d_d(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    real(dp),intent(out),dimension(:) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1
    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcvd(unit,column,frow,felem,n_rows,nullvald,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_1d_d')

  end subroutine read_table_column_1d_d

  subroutine read_table_column_2d_d(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    real(dp),intent(out),dimension(:,:) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1

    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcvd(unit,column,frow,felem,n_rows,nullvald,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_2d_d')

  end subroutine read_table_column_2d_d
  
  subroutine read_table_column_1d_name_alloc_d(unit,colname,values)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    real(dp),intent(out),allocatable,dimension(:) :: values
    integer :: n_rows,colnum
    integer,parameter :: frow = 1,felem = 1
    logical :: anyf
    integer :: status
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call fits_read_keyword(unit,'NAXIS2',n_rows)
    allocate(values(n_rows))
    call ftgcvd(unit,colnum,frow,felem,n_rows,nullvald,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_1d_name_alloc_d')
  end subroutine read_table_column_1d_name_alloc_d
  
  subroutine read_table_column_2d_name_alloc_d(unit,colname,values)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    real(dp),intent(out),allocatable,dimension(:,:) :: values
    integer :: n_rows,colnum,length
    integer,parameter :: frow = 1,felem = 1
    logical :: anyf
    integer :: status
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call fits_read_keyword(unit,'NAXIS2',n_rows)
    length = fits_table_column_width(unit,colnum)
    allocate(values(length,n_rows))
    call ftgcvd(unit,colnum,frow,felem,n_rows*length,nullvald,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_2d_name_alloc_d')
  end subroutine read_table_column_2d_name_alloc_d
  
  subroutine write_table_column_0d_d(unit,column,values,row)

    implicit none

    integer,intent(in) :: unit,column
    integer,parameter :: n_rows = 1
    ! the unit, column number, and expected number of rows
    
    real(dp),intent(in) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpcld(unit,column,frow,felem,n_rows,[values],status)
    call cfitsio_check_status(unit,status,'write_table_column_0d_d')

  end subroutine write_table_column_0d_d
  
  subroutine write_table_column_1d_d(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    real(dp),intent(in),dimension(:) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status
    
    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpcld(unit,column,frow,felem,n_rows,values,status)
    call cfitsio_check_status(unit,status,'write_table_column_1d_d')

  end subroutine write_table_column_1d_d
  
  subroutine write_table_column_2d_d(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    real(dp),intent(in),dimension(:,:) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status
    
    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpcld(unit,column,frow,felem,n_rows,values,status)
    call cfitsio_check_status(unit,status,'write_table_column_2d_d')

  end subroutine write_table_column_2d_d
  

  subroutine read_table_column_0d_e(unit,column,values,row)

    implicit none

    integer,intent(in) :: unit,column
    integer,parameter :: n_rows = 1
    ! the unit, column number, and expected number of rows
    
    real(sp),intent(out) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1
    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcve(unit,column,frow,felem,n_rows,nullvale,array0de,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_0d_e')

    values = array0de(1)

  end subroutine read_table_column_0d_e

  subroutine read_table_column_1d_e(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    real(sp),intent(out),dimension(:) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1
    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcve(unit,column,frow,felem,n_rows,nullvale,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_1d_e')

  end subroutine read_table_column_1d_e

  subroutine read_table_column_2d_e(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    real(sp),intent(out),dimension(:,:) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1

    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcve(unit,column,frow,felem,n_rows,nullvale,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_2d_e')

  end subroutine read_table_column_2d_e
  
  subroutine read_table_column_1d_name_alloc_e(unit,colname,values)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    real(sp),intent(out),allocatable,dimension(:) :: values
    integer :: n_rows,colnum
    integer,parameter :: frow = 1,felem = 1
    logical :: anyf
    integer :: status
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call fits_read_keyword(unit,'NAXIS2',n_rows)
    allocate(values(n_rows))
    call ftgcve(unit,colnum,frow,felem,n_rows,nullvale,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_1d_name_alloc_e')
  end subroutine read_table_column_1d_name_alloc_e
  
  subroutine read_table_column_2d_name_alloc_e(unit,colname,values)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    real(sp),intent(out),allocatable,dimension(:,:) :: values
    integer :: n_rows,colnum,length
    integer,parameter :: frow = 1,felem = 1
    logical :: anyf
    integer :: status
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call fits_read_keyword(unit,'NAXIS2',n_rows)
    length = fits_table_column_width(unit,colnum)
    allocate(values(length,n_rows))
    call ftgcve(unit,colnum,frow,felem,n_rows*length,nullvale,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_2d_name_alloc_e')
  end subroutine read_table_column_2d_name_alloc_e
  
  subroutine write_table_column_0d_e(unit,column,values,row)

    implicit none

    integer,intent(in) :: unit,column
    integer,parameter :: n_rows = 1
    ! the unit, column number, and expected number of rows
    
    real(sp),intent(in) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpcle(unit,column,frow,felem,n_rows,[values],status)
    call cfitsio_check_status(unit,status,'write_table_column_0d_e')

  end subroutine write_table_column_0d_e
  
  subroutine write_table_column_1d_e(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    real(sp),intent(in),dimension(:) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status
    
    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpcle(unit,column,frow,felem,n_rows,values,status)
    call cfitsio_check_status(unit,status,'write_table_column_1d_e')

  end subroutine write_table_column_1d_e
  
  subroutine write_table_column_2d_e(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    real(sp),intent(in),dimension(:,:) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status
    
    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpcle(unit,column,frow,felem,n_rows,values,status)
    call cfitsio_check_status(unit,status,'write_table_column_2d_e')

  end subroutine write_table_column_2d_e
  

  subroutine read_table_column_0d_j(unit,column,values,row)

    implicit none

    integer,intent(in) :: unit,column
    integer,parameter :: n_rows = 1
    ! the unit, column number, and expected number of rows
    
    integer,intent(out) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1
    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcvj(unit,column,frow,felem,n_rows,nullvalj,array0dj,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_0d_j')

    values = array0dj(1)

  end subroutine read_table_column_0d_j

  subroutine read_table_column_1d_j(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    integer,intent(out),dimension(:) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1
    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcvj(unit,column,frow,felem,n_rows,nullvalj,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_1d_j')

  end subroutine read_table_column_1d_j

  subroutine read_table_column_2d_j(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    integer,intent(out),dimension(:,:) :: values
    ! the values read in

    integer,optional :: row

    integer :: frow
    
    integer,parameter :: felem = 1

    logical :: anyf
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftgcvj(unit,column,frow,felem,n_rows,nullvalj,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_2d_j')

  end subroutine read_table_column_2d_j
  
  subroutine read_table_column_1d_name_alloc_j(unit,colname,values)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    integer,intent(out),allocatable,dimension(:) :: values
    integer :: n_rows,colnum
    integer,parameter :: frow = 1,felem = 1
    logical :: anyf
    integer :: status
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call fits_read_keyword(unit,'NAXIS2',n_rows)
    allocate(values(n_rows))
    call ftgcvj(unit,colnum,frow,felem,n_rows,nullvalj,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_1d_name_alloc_j')
  end subroutine read_table_column_1d_name_alloc_j
  
  subroutine read_table_column_2d_name_alloc_j(unit,colname,values)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    integer,intent(out),allocatable,dimension(:,:) :: values
    integer :: n_rows,colnum,length
    integer,parameter :: frow = 1,felem = 1
    logical :: anyf
    integer :: status
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call fits_read_keyword(unit,'NAXIS2',n_rows)
    length = fits_table_column_width(unit,colnum)
    allocate(values(length,n_rows))
    call ftgcvj(unit,colnum,frow,felem,n_rows*length,nullvalj,values,anyf,status)
    call cfitsio_check_status(unit,status,'read_table_column_2d_name_alloc_j')
  end subroutine read_table_column_2d_name_alloc_j
  
  subroutine write_table_column_0d_j(unit,column,values,row)

    implicit none

    integer,intent(in) :: unit,column
    integer,parameter :: n_rows = 1
    ! the unit, column number, and expected number of rows
    
    integer,intent(in) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status

    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpclj(unit,column,frow,felem,n_rows,[values],status)
    call cfitsio_check_status(unit,status,'write_table_column_0d_j')

  end subroutine write_table_column_0d_j
  
  subroutine write_table_column_1d_j(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    integer,intent(in),dimension(:) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status
    
    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpclj(unit,column,frow,felem,n_rows,values,status)
    call cfitsio_check_status(unit,status,'write_table_column_1d_j')

  end subroutine write_table_column_1d_j
  
  subroutine write_table_column_2d_j(unit,column,n_rows,values,row)

    implicit none

    integer,intent(in) :: unit,column,n_rows
    ! the unit, column number, and expected number of rows
    
    integer,intent(in),dimension(:,:) :: values
    ! the values write out

    integer,optional :: row

    integer :: frow
    integer,parameter :: felem = 1
    integer :: status
    
    if(present(row)) then
      frow = row
    else
      frow = 1
    end if

    status = 0

    call ftpclj(unit,column,frow,felem,n_rows,values,status)
    call cfitsio_check_status(unit,status,'write_table_column_2d_j')

  end subroutine write_table_column_2d_j
  
  
  
  subroutine fits_read_3d_array_d(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to read from

    integer :: nx,ny,nz
    ! image dimensions

    real(dp),dimension(:,:,:),intent(out) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    nx = size(array,1)
    ny = size(array,2)
    nz = size(array,3)
    
    status = 0
    
    call ftg3dd(unit,group,0.,nx,ny,nx,ny,nz,array,status)
 
    call cfitsio_check_status(unit,status,'fits_write_3d_array_d')

  end subroutine fits_read_3d_array_d
  
  subroutine fits_write_3d_array_d(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to write to

    integer :: nx,ny,nz
    ! image dimensions

    real(dp),dimension(:,:,:) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    nx = size(array,1)
    ny = size(array,2)
    nz = size(array,3)
    
    status = 0
    
    call ftp3dd(unit,group,nx,ny,nx,ny,nz,array,status)
 
    call cfitsio_check_status(unit,status,'fits_write_3d_array_d')

  end subroutine fits_write_3d_array_d

  subroutine fits_write_2d_array_d(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to write to

    integer :: nx,ny
    ! image dimensions

    real(dp),dimension(:,:) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    nx = size(array,1)
    ny = size(array,2)

    status = 0

    call ftp2dd(unit,group,nx,nx,ny,array,status)
    call cfitsio_check_status(unit,status,'fits_write_2d_array_d')

  end subroutine fits_write_2d_array_d

  subroutine fits_read_2d_array_d(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to write to

    integer :: nx,ny
    ! image dimensions

    real(dp),dimension(:,:) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    logical :: anyf
    real,parameter :: nullval = 0.

    nx = size(array,1)
    ny = size(array,2)

    status = 0

    call ftg2dd(unit,group,nullval,nx,nx,ny,array,anyf,status)
    call cfitsio_check_status(unit,status,'fits_read_2d_array_d')

  end subroutine fits_read_2d_array_d

  
  subroutine fits_read_3d_array_e(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to read from

    integer :: nx,ny,nz
    ! image dimensions

    real(sp),dimension(:,:,:),intent(out) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    nx = size(array,1)
    ny = size(array,2)
    nz = size(array,3)
    
    status = 0
    
    call ftg3de(unit,group,0.,nx,ny,nx,ny,nz,array,status)
 
    call cfitsio_check_status(unit,status,'fits_write_3d_array_e')

  end subroutine fits_read_3d_array_e
  
  subroutine fits_write_3d_array_e(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to write to

    integer :: nx,ny,nz
    ! image dimensions

    real(sp),dimension(:,:,:) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    nx = size(array,1)
    ny = size(array,2)
    nz = size(array,3)
    
    status = 0
    
    call ftp3de(unit,group,nx,ny,nx,ny,nz,array,status)
 
    call cfitsio_check_status(unit,status,'fits_write_3d_array_e')

  end subroutine fits_write_3d_array_e

  subroutine fits_write_2d_array_e(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to write to

    integer :: nx,ny
    ! image dimensions

    real(sp),dimension(:,:) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    nx = size(array,1)
    ny = size(array,2)

    status = 0

    call ftp2de(unit,group,nx,nx,ny,array,status)
    call cfitsio_check_status(unit,status,'fits_write_2d_array_e')

  end subroutine fits_write_2d_array_e

  subroutine fits_read_2d_array_e(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to write to

    integer :: nx,ny
    ! image dimensions

    real(sp),dimension(:,:) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    logical :: anyf
    real,parameter :: nullval = 0.

    nx = size(array,1)
    ny = size(array,2)

    status = 0

    call ftg2de(unit,group,nullval,nx,nx,ny,array,anyf,status)
    call cfitsio_check_status(unit,status,'fits_read_2d_array_e')

  end subroutine fits_read_2d_array_e

  
  subroutine fits_read_3d_array_j(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to read from

    integer :: nx,ny,nz
    ! image dimensions

    integer,dimension(:,:,:),intent(out) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    nx = size(array,1)
    ny = size(array,2)
    nz = size(array,3)
    
    status = 0
    
    call ftg3dj(unit,group,0.,nx,ny,nx,ny,nz,array,status)
 
    call cfitsio_check_status(unit,status,'fits_write_3d_array_j')

  end subroutine fits_read_3d_array_j
  
  subroutine fits_write_3d_array_j(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to write to

    integer :: nx,ny,nz
    ! image dimensions

    integer,dimension(:,:,:) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    nx = size(array,1)
    ny = size(array,2)
    nz = size(array,3)
    
    status = 0
    
    call ftp3dj(unit,group,nx,ny,nx,ny,nz,array,status)
 
    call cfitsio_check_status(unit,status,'fits_write_3d_array_j')

  end subroutine fits_write_3d_array_j

  subroutine fits_write_2d_array_j(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to write to

    integer :: nx,ny
    ! image dimensions

    integer,dimension(:,:) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    nx = size(array,1)
    ny = size(array,2)

    status = 0

    call ftp2dj(unit,group,nx,nx,ny,array,status)
    call cfitsio_check_status(unit,status,'fits_write_2d_array_j')

  end subroutine fits_write_2d_array_j

  subroutine fits_read_2d_array_j(unit,array)

    implicit none

    integer,intent(in) :: unit
    ! the unit of the file to write to

    integer :: nx,ny
    ! image dimensions

    integer,dimension(:,:) :: array
    ! the array

    integer,parameter :: group = 1
    integer :: status

    logical :: anyf
    real,parameter :: nullval = 0.

    nx = size(array,1)
    ny = size(array,2)

    status = 0

    call ftg2dj(unit,group,nullval,nx,nx,ny,array,anyf,status)
    call cfitsio_check_status(unit,status,'fits_read_2d_array_j')

  end subroutine fits_read_2d_array_j


  logical function fits_exists_keyword(unit,name)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: name
    character(len=100) :: value,comment
    integer :: status
    status=0
    call ftgkey(unit,name,value,comment,status)
    select case(status)
    case(0)
       fits_exists_keyword = .true.
    case(202)
       fits_exists_keyword = .false.
    case default
       call cfitsio_check_status(unit,status,'fits_exists_keyword')
    end select
  end function fits_exists_keyword


  subroutine fits_read_kl(unit,name,value)

    implicit none

    integer,intent(in) :: unit
    ! the file unit

    character(len=*),intent(in) :: name
    ! the name of the keyword

    logical,intent(out) :: value
    ! the value to read in

    character(len=100) :: comment
    integer :: status

    if(verbose) then
       write(*,'("[base_cfitsio] reading ")',advance='no')
       write(*,*) trim(name)
    end if

    status = 0

    call ftgkyl(unit,name,value,comment,status)
    call cfitsio_check_status(unit,status,'fits_read_kl')

  end subroutine fits_read_kl


  subroutine fits_read_ks(unit,name,value)

    implicit none

    integer,intent(in) :: unit
    ! the file unit

    character(len=*),intent(in) :: name
    ! the name of the keyword

    character(len=*),intent(out) :: value
    ! the value to read in

    character(len=100) :: comment
    integer :: status

    if(verbose) then
       write(*,'("[base_cfitsio] reading ")',advance='no')
       write(*,*) trim(name)
    end if

    status = 0

    call ftgkys(unit,name,value,comment,status)
    call cfitsio_check_status(unit,status,'fits_read_ks')

  end subroutine fits_read_ks


  subroutine fits_read_kd(unit,name,value)

    implicit none

    integer,intent(in) :: unit
    ! the file unit

    character(len=*),intent(in) :: name
    ! the name of the keyword

    real(dp),intent(out) :: value
    ! the value to read in

    character(len=100) :: comment
    integer :: status

    if(verbose) then
       write(*,'("[base_cfitsio] reading ")',advance='no')
       write(*,*) trim(name)
    end if

    status = 0

    call ftgkyd(unit,name,value,comment,status)
    call cfitsio_check_status(unit,status,'fits_read_kd')

  end subroutine fits_read_kd


  subroutine fits_read_ke(unit,name,value)

    implicit none

    integer,intent(in) :: unit
    ! the file unit

    character(len=*),intent(in) :: name
    ! the name of the keyword

    real(sp),intent(out) :: value
    ! the value to read in

    character(len=100) :: comment
    integer :: status

    if(verbose) then
       write(*,'("[base_cfitsio] reading ")',advance='no')
       write(*,*) trim(name)
    end if

    status = 0

    call ftgkye(unit,name,value,comment,status)
    call cfitsio_check_status(unit,status,'fits_read_ke')

  end subroutine fits_read_ke


  subroutine fits_read_kj(unit,name,value)

    implicit none

    integer,intent(in) :: unit
    ! the file unit

    character(len=*),intent(in) :: name
    ! the name of the keyword

    integer,intent(out) :: value
    ! the value to read in

    character(len=100) :: comment
    integer :: status

    if(verbose) then
       write(*,'("[base_cfitsio] reading ")',advance='no')
       write(*,*) trim(name)
    end if

    status = 0

    call ftgkyj(unit,name,value,comment,status)
    call cfitsio_check_status(unit,status,'fits_read_kj')

  end subroutine fits_read_kj



  subroutine fits_write_kl(unit,name,value,comment)

    implicit none

    integer,intent(in) :: unit
    ! the file unit

    character(len=*),intent(in) :: name
    ! the name of the keyword

    logical,intent(in) :: value
    ! the value to write out

    character(len=*),intent(in),optional :: comment
    integer :: status

    status = 0
    
    if(present(comment)) then
      call ftukyl(unit,name,value,comment,status)
    else  
      call ftukyl(unit,name,value,"",status)
    end if
    
    call cfitsio_check_status(unit,status,'fits_write_kl')

  end subroutine fits_write_kl


  subroutine fits_write_ks(unit,name,value,comment)

    implicit none

    integer,intent(in) :: unit
    ! the file unit

    character(len=*),intent(in) :: name
    ! the name of the keyword

    character(len=*),intent(in) :: value
    ! the value to write out

    character(len=*),intent(in),optional :: comment
    integer :: status

    status = 0
    
    if(present(comment)) then
      call ftukys(unit,name,value,comment,status)
    else  
      call ftukys(unit,name,value,"",status)
    end if
    
    call cfitsio_check_status(unit,status,'fits_write_ks')

  end subroutine fits_write_ks


  subroutine fits_write_kj(unit,name,value,comment)

    implicit none

    integer,intent(in) :: unit
    ! the file unit

    character(len=*),intent(in) :: name
    ! the name of the keyword

    integer,intent(in) :: value
    ! the value to write out

    character(len=*),intent(in),optional :: comment
    integer :: status

    status = 0
    
    if(present(comment)) then
      call ftukyj(unit,name,value,comment,status)
    else  
      call ftukyj(unit,name,value,"",status)
    end if
    
    call cfitsio_check_status(unit,status,'fits_write_kj')

  end subroutine fits_write_kj



  subroutine fits_write_kd(unit,name,value,comment)

    implicit none

    integer,intent(in) :: unit
    ! the file unit

    character(len=*),intent(in) :: name
    ! the name of the keyword

    real(dp),intent(in) :: value
    ! the value to write out

    character(len=*),intent(in),optional :: comment
    integer :: status

    status = 0

    if(present(comment)) then
      call ftukyd(unit,name,value,10,comment,status)
    else
      call ftukyd(unit,name,value,10,"",status)
    end if
    
    call cfitsio_check_status(unit,status,'fits_write_kd')

  end subroutine fits_write_kd


  subroutine fits_write_ke(unit,name,value,comment)

    implicit none

    integer,intent(in) :: unit
    ! the file unit

    character(len=*),intent(in) :: name
    ! the name of the keyword

    real(sp),intent(in) :: value
    ! the value to write out

    character(len=*),intent(in),optional :: comment
    integer :: status

    status = 0

    if(present(comment)) then
      call ftukye(unit,name,value,10,comment,status)
    else
      call ftukye(unit,name,value,10,"",status)
    end if
    
    call cfitsio_check_status(unit,status,'fits_write_ke')

  end subroutine fits_write_ke

  
  integer function fits_table_column_number(unit,name)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: name
    integer :: status
    status = 0
    call ftgcno(unit,.true.,name,fits_table_column_number,status)
    call cfitsio_check_status(unit,status,'fits_table_column_number')
  end function fits_table_column_number
  
  integer function fits_table_number_columns(unit)
    implicit none
    integer,intent(in) :: unit
    integer :: status
    status = 0
    call ftgncl(unit,fits_table_number_columns,status)
    call cfitsio_check_status(unit,status,'fits_table_number_columns')
  end function fits_table_number_columns
  
  integer function fits_table_number_rows(unit)
    implicit none
    integer,intent(in) :: unit
    integer :: status
    status = 0
    call ftgnrw(unit,fits_table_number_rows,status)
    call cfitsio_check_status(unit,status,'fits_table_number_rows')
  end function fits_table_number_rows

  subroutine fits_table_new_column(unit,colnum,ttype,tform)
    implicit none
    integer,intent(in)          :: unit,colnum
    character(len=*),intent(in) :: ttype,tform
    integer                     :: status
    status = 0
    call fticol(unit,colnum,ttype,tform,status)
    call cfitsio_check_status(unit,status,'fits_table_number_rows')
  end subroutine fits_table_new_column
  
    
  subroutine fits_write_array_line_b(unit,line,nx,array)
    implicit none
    integer,intent(in) :: unit,line,nx
    character(len=1),intent(in)      :: array(:)
    integer,parameter  :: group = 1
    integer            :: status,first_pixel
    status = 0
    first_pixel = (line-1)*nx+1
    call ftpprb(unit,group,first_pixel,nx,array,status)
    call cfitsio_check_status(unit,status,'fits_write_array_b')
  end subroutine fits_write_array_line_b
  
  subroutine fits_read_array_line_b(unit,line,nx,array)
    implicit none
    integer,intent(in) :: unit,line,nx
    character(len=1),intent(out)     :: array(:)
    integer,parameter  :: group = 1
    integer            :: status,first_pixel
    logical            :: anyf
    logical,dimension(size(array)) :: flagvals
    status = 0
    first_pixel = (line-1)*nx+1
    call ftgpfb(unit,group,first_pixel,nx,array,flagvals,anyf,status)
    call cfitsio_check_status(unit,status,'fits_read_array_b')
  end subroutine fits_read_array_line_b
  
    
  subroutine fits_write_array_line_d(unit,line,nx,array)
    implicit none
    integer,intent(in) :: unit,line,nx
    real(dp),intent(in)      :: array(:)
    integer,parameter  :: group = 1
    integer            :: status,first_pixel
    status = 0
    first_pixel = (line-1)*nx+1
    call ftpprd(unit,group,first_pixel,nx,array,status)
    call cfitsio_check_status(unit,status,'fits_write_array_d')
  end subroutine fits_write_array_line_d
  
  subroutine fits_read_array_line_d(unit,line,nx,array)
    implicit none
    integer,intent(in) :: unit,line,nx
    real(dp),intent(out)     :: array(:)
    integer,parameter  :: group = 1
    integer            :: status,first_pixel
    logical            :: anyf
    logical,dimension(size(array)) :: flagvals
    status = 0
    first_pixel = (line-1)*nx+1
    call ftgpfd(unit,group,first_pixel,nx,array,flagvals,anyf,status)
    call cfitsio_check_status(unit,status,'fits_read_array_d')
  end subroutine fits_read_array_line_d
  
    
  subroutine fits_write_array_line_e(unit,line,nx,array)
    implicit none
    integer,intent(in) :: unit,line,nx
    real(sp),intent(in)      :: array(:)
    integer,parameter  :: group = 1
    integer            :: status,first_pixel
    status = 0
    first_pixel = (line-1)*nx+1
    call ftppre(unit,group,first_pixel,nx,array,status)
    call cfitsio_check_status(unit,status,'fits_write_array_e')
  end subroutine fits_write_array_line_e
  
  subroutine fits_read_array_line_e(unit,line,nx,array)
    implicit none
    integer,intent(in) :: unit,line,nx
    real(sp),intent(out)     :: array(:)
    integer,parameter  :: group = 1
    integer            :: status,first_pixel
    logical            :: anyf
    logical,dimension(size(array)) :: flagvals
    status = 0
    first_pixel = (line-1)*nx+1
    call ftgpfe(unit,group,first_pixel,nx,array,flagvals,anyf,status)
    call cfitsio_check_status(unit,status,'fits_read_array_e')
  end subroutine fits_read_array_line_e
  
  
  
end module base_cfitsio
