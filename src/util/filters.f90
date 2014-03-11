!#############################################################################
! Purpose : module and subroutine for reading in filt
! Author  : Thomas Robitaille
! Date    : 14th September 2006
! Checked : Yes
! Known Issues: none
!#############################################################################

module base_filters

  implicit none
  save

  type filter
    character(len=5) :: name = ""
    real :: wavelength_microns = 0.
    logical :: do = .false.
    real,allocatable :: wa(:)
    real,allocatable :: nu(:)
    real,allocatable :: va(:)
    real,allocatable :: t_va(:)
  end type    

contains

  subroutine read_filters(parameter_file,filters)
    use base_io
    use base_constants
    implicit none
    character(len=*),intent(in) :: parameter_file
    integer :: unit
    integer :: n_filt,f
    type(filter),allocatable,intent(inout) :: filters(:)
    logical,allocatable :: do(:)
    
    n_filt = file_n_lines(parameter_file) - 1
    allocate(do(n_filt))
    call open_safe(unit,file=parameter_file,status='old')
    read(unit,*)
    do f=1,n_filt
       read(unit,*) do(f)
    end do
    close(unit)

    n_filt = count(do)
       
    allocate(filters(n_filt))

    call open_safe(unit,file=parameter_file,status='old')
    read(unit,*)
    f = 1
    do
      read(unit,*) filters(f)%do,filters(f)%name,filters(f)%wavelength_microns
      if(filters(f)%do) then
         call read_column("config/filters/"//trim(filters(f)%name)//".txt",1,filters(f)%wa)
         call read_column("config/filters/"//trim(filters(f)%name)//".txt",2,filters(f)%va)
         allocate(filters(f)%nu(size(filters(f)%wa)))
         filters(f)%nu=c_si/(filters(f)%wa*microns2m)
         f = f + 1
         if(f > n_filt) exit
      end if
    end do
    close(unit)

  end subroutine read_filters

  subroutine rebin_filters(filters,x_new)

    use base_array
    implicit none

    type(filter),intent(inout) :: filters(:)
    real,intent(in)  :: x_new(:)
    integer :: j
    ! loop variable
    real :: x_min,x_max
    ! the range of wavelengths going into a target wavelength

    integer :: f

    do f=1,size(filters)

      if(allocated(filters(f)%t_va)) deallocate(filters(f)%t_va)
      allocate(filters(f)%t_va(size(x_new)))

      filters(f)%t_va=0.

      x_min=x_new(1)
      x_max=0.5*(x_new(2)+x_new(1))

      filters(f)%t_va(1)=integral(filters(f)%nu,filters(f)%va,x_min,x_max)

      do j=2,size(x_new)-1

        x_min=0.5*(x_new(j)+x_new(j-1))
        x_max=0.5*(x_new(j)+x_new(j+1))

        filters(f)%t_va(j)=integral(filters(f)%nu,filters(f)%va,x_min,x_max)

      end do

      x_min=0.5*(x_new(size(x_new)-1)+x_new(size(x_new)))
      x_max=x_new(size(x_new))

      filters(f)%t_va(size(x_new))=integral(filters(f)%nu,filters(f)%va,x_min,x_max)

    end do

  end subroutine rebin_filters

end module base_filters
