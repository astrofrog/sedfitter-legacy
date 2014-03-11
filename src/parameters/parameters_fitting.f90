module parameters_fitting

  use base_types  
  use base_messages
  use base_parfile
  use base_io
  
  implicit none
  save
  
  private
  public :: read_fitting_parameters
  public :: check_fitting_parameters_set
  
  logical,private :: set = .false.
  ! whether the parameters have been set
  
  real(sp),public :: d_kpc_min = -huge(d_kpc_min)
  real(sp),public :: d_kpc_max = -huge(d_kpc_min)
  ! Minimum and maximum distance

  real(sp),public :: av_min = -huge(av_min)
  real(sp),public :: av_max = -huge(av_max)
  ! The Av range allowed in fitting
  
  character(len=1000),public :: filename_ex_law
  ! Extinction law filename
  
  character(len=1000),public :: models_directory
  ! Directory with all the models
  
  logical,public :: keep_extended
  ! Whether to keep extended models
  
contains
  
  subroutine read_fitting_parameters(filename,distance_required)
    implicit none
    character(len=*),intent(in) :: filename
    logical,intent(in) :: distance_required
    call message_section("Fitting parameters")
    call load_par_file(filename)
    call read_par('exlaw',filename_ex_law)
    if(present_arg('models')) then
      models_directory = char_arg('models')
    else
      call read_par('modir',models_directory)
    end if
    call check_dir_exists(models_directory)
    call read_par('minav',av_min)
    call read_par('maxav',av_max)
    write(*,'(1X,A19,F9.3,A4)') "Minimum A_V      : ",av_min,' mag'
    write(*,'(1X,A19,F9.3,A4)') "Maximum A_V      : ",av_max,' mag'
    if(distance_required) then
      call read_par('mind',d_kpc_min)
      call read_par('maxd',d_kpc_max)
      write(*,'(1X,A19,F9.3,A4)') "Minimum distance : ",d_kpc_min,' kpc'
      write(*,'(1X,A19,F9.3,A4)') "Maximum distance : ",d_kpc_max,' kpc'
      call read_par('kpext',keep_extended)
   end if
    set = .true.
  end subroutine read_fitting_parameters
  
  subroutine check_fitting_parameters_set(origin)
    implicit none
    character(len=*),intent(in) :: origin
    if(.not.set) call error(origin,"fitting parameters not set")
  end subroutine check_fitting_parameters_set
  
end module parameters_fitting
