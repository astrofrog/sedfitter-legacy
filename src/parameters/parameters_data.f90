module parameters_data

  use base_messages
  use base_parfile
  use base_source
  use base_io
  
  implicit none
  save

  private
  public :: read_data_parameters
  public :: check_data_parameters_set

  logical,private :: set = .false.
  ! whether the parameters have been set
  
  integer,public   :: n_wav   = 0
  ! number of wavelengths
  
  type(filter),allocatable,public :: filt(:)
  ! The filters the data was taken in
  
  character(len=1000),public :: data_file
  ! data file
  
  integer,public :: min_data
  ! minimum required number of datapoints
  
contains

  subroutine read_data_parameters(filename)
    implicit none
    character(len=*),intent(in) :: filename
    character(len=1000)         :: data_file_format
    integer :: unit,j
    call message_section("Data format parameters")
    call load_par_file(filename)
    if(present_arg('input_form')) then
      data_file_format = char_arg('input_form')
    else
      call read_par('dform',data_file_format)
    end if
    if(present_arg('input')) then
      data_file = char_arg('input')
    else
      call read_par('dfile',data_file)
    end if
    call read_par('drequ',min_data)
    call open_safe(unit,data_file_format,status='old')
    read(unit,*)
    read(unit,*) n_wav
    allocate(filt(n_wav))
    read(unit,*) (filt(j)%name,j=1,n_wav)
    read(unit,*) (filt(j)%aperture_arcsec,j=1,n_wav)
    close(unit)
    write(*,*) "Number of filters : ",n_wav
    set = .true.
  end subroutine read_data_parameters
  
  subroutine check_data_parameters_set(origin)
    implicit none
    character(len=*),intent(in) :: origin
    if(.not.set) call error(origin,"data parameters not set")
  end subroutine check_data_parameters_set

end module parameters_data

