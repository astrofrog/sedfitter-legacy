module base_source

  use posix
  use base_types

  implicit none
  save

  private
  public :: source,filter
  public :: set_source_size
  public :: print_source,print_filters
  public :: read_source_ascii
  public :: write_source_ascii
  public :: check_source_allocation

  type filter
    character(len=5) :: name
    real(sp) :: wavelength_microns
    real(sp) :: aperture_arcsec
  end type filter

  type source
    integer :: n_wav,n_data
    character(len=30) :: name
    real(dp) :: x,y
    integer,allocatable,dimension(:) :: valid
    real(sp),allocatable,dimension(:) :: flux,flux_error,weight
    real(sp),allocatable,dimension(:) :: log_flux,log_flux_error
  end type source

contains

  subroutine set_source_size(s,n)
    implicit none
    type(source),intent(inout) :: s
    integer,intent(in) :: n
    allocate(s%valid(n))
    allocate(s%flux(n))
    allocate(s%flux_error(n))
    allocate(s%weight(n))
    allocate(s%log_flux(n))
    allocate(s%log_flux_error(n))
    s%n_wav = n
  end subroutine set_source_size

  subroutine check_source_allocation(s)
    use base_messages, only : error
    implicit none
    type(source),intent(in) :: s
    if(.not.allocated(s%valid))           call error("check_allocated_source","valid array not allocated")
    if(.not.allocated(s%flux))            call error("check_allocated_source","flux array not allocated")
    if(.not.allocated(s%flux_error))      call error("check_allocated_source","flux_error array not allocated")
    if(.not.allocated(s%weight))          call error("check_allocated_source","weight array not allocated")
    if(.not.allocated(s%log_flux))        call error("check_allocated_source","log_flux array not allocated")
    if(.not.allocated(s%log_flux_error))  call error("check_allocated_source","log_flux_error array not allocated")
    if(size(s%valid).ne.s%n_wav)          call error("check_allocated_source","valid array has incorrect size")
    if(size(s%flux).ne.s%n_wav)           call error("check_allocated_source","flux array has incorrect size")
    if(size(s%flux_error).ne.s%n_wav)     call error("check_allocated_source","flux_error array has incorrect size")
    if(size(s%weight).ne.s%n_wav)         call error("check_allocated_source","weight array has incorrect size")
    if(size(s%log_flux).ne.s%n_wav)       call error("check_allocated_source","log_flux array has incorrect size")
    if(size(s%log_flux_error).ne.s%n_wav) call error("check_allocated_source","log_flux_error array has incorrect size")
  end subroutine check_source_allocation

  subroutine print_filters(f)
    implicit none
    type(filter),intent(in) :: f(:)
    integer :: j
    write(*,'(6X,"  Filter    Wavelength    Aperture (",A1,")")') '"'
    write(*,'(6X," ---------------------------------------- ")')
    do j=1,size(f)
      write(*,'(10X,A5,2(5X,F7.2))') f(j)%name,f(j)%wavelength_microns,f(j)%aperture_arcsec
    end do
  end subroutine print_filters
      
  subroutine print_source(s)
    implicit none
    type(source),intent(in) :: s
    integer :: j
    write(*,'("Source name : ",a30)') s%name
    write(*,'("RA   / l    : ",F9.5)') s%x
    write(*,'("Decl / b    : ",F9.5)') s%y
    do j=1,s%n_wav
      write(*,'("F = ",ES12.4," +/- ",ES12.4," mJy (",I1,")  ")',advance='no') s%flux(j),s%flux_error(j),s%valid(j)
      write(*,'("Log[F] = ",F8.5,"+/-",F8.5)') s%log_flux(j),s%log_flux_error(j)
    end do
  end subroutine print_source

  subroutine write_source_ascii(unit,s)
    implicit none
    integer,intent(in) :: unit   ! the file unit to write to
    type(source),intent(in) :: s ! the source to write out
    character(len=100) :: fmt    ! output format
    integer :: j                 ! loop variable
    call check_source_allocation(s)
    call check_source_data(s)
    write(fmt,'("(A30,2(1X,F9.5),",I3.3,"(1X,I1),",I3.3,"(2(1X,ES11.4)))")') s%n_wav,s%n_wav
    write(unit,fmt) s%name,s%x,s%y,s%valid(:),(s%flux(j),s%flux_error(j),j=1,s%n_wav)
    call flush(unit)
  end subroutine write_source_ascii

  subroutine read_source_ascii(unit,s,noform)
    use base_messages,only : error
    implicit none
    integer,intent(in) :: unit    ! the file unit to write to
    type(source),intent(inout) :: s ! the source to read in
    logical,intent(in),optional :: noform ! whether to use a format to read in
    character(len=100) :: fmt     ! output format
    integer :: j                  ! loop variable
    integer :: ioerr              ! used for I/O errors
    call check_source_allocation(s)
    write(fmt,'("(A30,2(1X,F9.5),",I3.3,"(1X,I1),",I3.3,"(2(1X,ES11.4)))")') s%n_wav,s%n_wav
    if(.not.present(noform)) then
      read(unit,fmt,iostat=ioerr) s%name,s%x,s%y,s%valid(:),(s%flux(j),s%flux_error(j),j=1,s%n_wav)
    else
      if(noform) then
        read(unit,*,iostat=ioerr) s%name,s%x,s%y,s%valid(:),(s%flux(j),s%flux_error(j),j=1,s%n_wav)
      else
        read(unit,fmt,iostat=ioerr) s%name,s%x,s%y,s%valid(:),(s%flux(j),s%flux_error(j),j=1,s%n_wav)
      end if
    end if
    if(ioerr.ne.0) call error("read_source_ascii","error while reading in data file")
    call check_source_data(s)
    call find_log_fluxes(s)
    s%n_data = count(s%valid==1.or.s%valid==4)
    s%name = adjustl(s%name)
  end subroutine read_source_ascii

  subroutine check_source_data(s)
    use base_messages, only : error
    implicit none
    type(source),intent(in) :: s
    integer :: j
    do j=1,s%n_wav
      select case(s%valid(j))
      case(0,1,4)
        ! nothing to do
      case(2,3)
        if(s%flux_error(j) < 0. .or. s%flux_error(j) > 1.) then
          call error("check_source","confidence limit for lower/upper limit out of bounds")
        end if
      case default
        call error("check_source","invalid flag")
      end select
    end do
  end subroutine check_source_data
  
  subroutine find_log_fluxes(s)
    implicit none
    type(source),intent(inout) :: s
    where(s%valid==0)
      s%log_flux       = 0.
      s%log_flux_error = 0.
      s%weight         = 0.
    elsewhere(s%valid==1) ! Valid linear flux
      s%log_flux       = log10(s%flux)-0.5*(s%flux_error/s%flux)**2/log(10.)
      s%log_flux_error = abs(s%flux_error/s%flux)/log(10.)
      s%weight         = 1./s%log_flux_error**2.
    elsewhere(s%valid==2.or.s%valid==3) ! Upper or lower limit
      s%log_flux       = log10(s%flux)
      s%log_flux_error = s%flux_error ! Error is confidence (between 0 and 1)
      s%weight         = 0.
    elsewhere(s%valid==4) ! Valid log10 flux
      s%log_flux       = s%flux
      s%log_flux_error = s%flux_error
      s%weight         = 1./s%log_flux_error**2.
    end where
  end subroutine find_log_fluxes

end module base_source


