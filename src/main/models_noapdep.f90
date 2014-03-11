module SED_models

  use base_types

  implicit none
  save
  
  private
  public :: setup_models
  
  logical,parameter,public :: distance_required = .false.
  
  integer,allocatable,public           :: model_id(:)

  character(len=30),allocatable,public :: model_names(:) 
  real(sp),allocatable,public :: model_fluxes(:,:)
  ! names and fluxes of all the models

contains

  subroutine setup_models()

    use base_messages, only              : message_section, error
    
    use parameters_data, only            : filt, n_wav, check_data_parameters_set
    use parameters_models, only          : check_model_parameters_set
    use parameters_fitting, only         : models_directory, check_fitting_parameters_set
    
    use base_fits_convolved_fluxes, only : interpolate_convolved_fluxes, read_convolved_fluxes
    
    implicit none
 
    character(len=1000) :: filename
    ! FITS file name

    integer :: j,m
    ! loop variables

    real(sp),allocatable :: temp_fluxes(:,:)
    ! temporary storage for fluxes
            
    call check_fitting_parameters_set("setup_models")
    call check_model_parameters_set("setup_models")
    call check_data_parameters_set("setup_models")
        
    call message_section("Reading in convolved fluxes")

    ! --- Loop over wavelenths and load models --- !

    do j=1,n_wav

       filename = trim(models_directory)//'/convolved/'//trim(filt(j)%name)//'.fits'
              
       call read_convolved_fluxes(filename,fluxes=temp_fluxes,wavelength=filt(j)%wavelength_microns)
       
       if(j==1) then
         allocate(model_fluxes(size(temp_fluxes,2),n_wav))
       end if
       
       model_fluxes(:,j) = temp_fluxes(1,:)
       
       if(j==1) then
          call read_convolved_fluxes(filename,model_names=model_names)
          allocate(model_id(size(model_names)))
       end if

    end do

    forall(m=1:size(model_id)) model_id(m) = m
    
    write(*,*)
        
    where(model_fluxes == 0.)
      model_fluxes = -huge(0.)
    elsewhere 
      model_fluxes = log10(model_fluxes)
    end where
            
  end subroutine setup_models
  
end module SED_models



 
