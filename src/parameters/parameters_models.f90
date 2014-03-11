module parameters_models

  use base_types
  use base_config
  use base_messages
  
  implicit none
  save
  
  private
  public :: read_model_parameters
  public :: check_model_parameters_set
  public :: get_filename_sed
  
  logical,private :: set = .false.
  ! whether the parameters have been set
   
  real(sp),public :: models_logd_step
  
  integer,private :: length_subdir
  
  logical :: aperture_dependent

contains
  
  subroutine get_filename_sed(models_directory,model_name,filename_sed)
    implicit none
    character(len=*),intent(in)  :: models_directory,model_name
    character(len=*),intent(out) :: filename_sed
    character(len=len(model_name)) :: subdir
    call check_model_parameters_set("get_filename_sed")
    if(length_subdir==0) then
      filename_sed = trim(models_directory)//'/seds/'//trim(model_name)//'_sed.fits'
    else
      subdir = model_name(1:length_subdir)
      filename_sed = trim(models_directory)//'/seds/'//trim(subdir)//'/'//trim(model_name)//'_sed.fits'
    end if
  end subroutine get_filename_sed
  
  subroutine read_model_parameters(models_directory,distance_required)
    implicit none
    character(len=*),intent(in) :: models_directory
    character(len=1000)         :: models_description
    logical,intent(in),optional :: distance_required
    call message_section("Model parameters")
    call load_config_file(trim(models_directory)//'/models.conf')
    call read_config('name',models_description)
    call read_config('length_subdir',length_subdir)
    call read_config('aperture_dependent',aperture_dependent)
    write(*,'(" Models            : ")',advance='no')
    write(*,*) trim(models_description)
    if(present(distance_required)) then
      if(.not.(distance_required.eqv.aperture_dependent)) then
        if(distance_required) then
          call error("read_model_parameters","models are not aperture-dependent, use bin/fit_stellar instead")
        else
          call error("read_model_parameters","models are aperture-dependent, use bin/fit instead")
        end if
      end if
      if(distance_required) then
        call read_config('logd_step',models_logd_step)
        write(*,'(" Log[d] stepping   :  ",F6.3)') models_logd_step
      end if
    end if
    set = .true.
  end subroutine read_model_parameters
  
  subroutine check_model_parameters_set(origin)
    implicit none
    character(len=*),intent(in) :: origin
    if(.not.set) call error(origin,"model parameters not set")
  end subroutine check_model_parameters_set
  
end module parameters_models
