module SED_models

  use base_types
  
  implicit none
  save

  private
  public :: setup_models
  
  logical,parameter,public :: distance_required = .true.

  real(sp),allocatable,private :: distances_kpc(:)
  real(sp),allocatable,public  :: log_distances_kpc(:)
  
  integer,allocatable,public           :: model_id(:)
  character(len=30),allocatable,public :: model_names(:) 

  real(sp),allocatable,public     :: model_fluxes(:,:,:)
  logical(1),allocatable,public  :: extended(:,:,:)
  logical(1),allocatable,private :: resolved(:,:,:)

contains

  subroutine setup_models()

    use base_messages, only              : message_section, error
    
    use parameters_data, only            : filt, n_wav, check_data_parameters_set
    use parameters_models, only          : models_logd_step, check_model_parameters_set
    use parameters_fitting, only         : models_directory, d_kpc_min, d_kpc_max, &
                                         & check_fitting_parameters_set
    
    use base_fits_convolved_fluxes, only : interpolate_convolved_fluxes, &
                                         & read_convolved_fluxes
    
    implicit none
 
    real(sp) :: logd_min
    real(sp) :: logd_max
    ! log10 values for the distance range
        
    character(len=1000) :: filename
    ! FITS file name

    real(sp),allocatable :: apertures_au(:)
    ! apertures in AU for a given wavelength

    integer :: j,d,m
    ! loop variables
    
    integer :: n_distances
    ! number of distances to compute models for
    
    integer :: n_models
    ! number of models from each file
            
    call check_fitting_parameters_set("setup_models")
    call check_model_parameters_set("setup_models")
    call check_data_parameters_set("setup_models")
    
    ! Set up distances to compute models for
  
    logd_min = log10(d_kpc_min) ; logd_max = log10(d_kpc_max)
    
    n_distances = max(ceiling( (logd_max-logd_min) / models_logd_step),5)

    write(*,'(" # of distances    :  ",I0)') n_distances

    allocate(distances_kpc(n_distances))
    allocate(apertures_au(n_distances))
    allocate(log_distances_kpc(n_distances))

    forall(d=1:n_distances)
      log_distances_kpc(d) = real(d-1)/real(n_distances-1) * (logd_max-logd_min) + logd_min
      distances_kpc(d)    = 10.**log_distances_kpc(d)
    end forall
    
    call message_section("Reading in convolved fluxes")

    ! --- Loop over wavelenths and load models --- !

    do j=1,n_wav

       filename = trim(models_directory)//'/convolved/'//trim(filt(j)%name)//'.fits'
              
       apertures_au = filt(j)%aperture_arcsec * distances_kpc * 1000.
       
       if(j==1) then
         call read_convolved_fluxes(filename,model_names=model_names)
         n_models = size(model_names)
         allocate(model_id(n_models))
         write(*,'("RAM required : ",F7.1,"MB")') n_models*n_wav*n_distances*6./1024.**2
         allocate(model_fluxes(n_models,n_wav,n_distances))
         allocate(resolved(n_models,n_wav,n_distances))
         allocate(extended(n_models,n_wav,n_distances))
       end if
         
       call interpolate_convolved_fluxes(filename,apertures_au,model_fluxes(:,j,:), &
         &                               resolved(:,j,:),extended(:,j,:), & 
         &                               wavelength=filt(j)%wavelength_microns)
       
    end do

    forall(m=1:n_models) model_id(m) = m
        
    forall(d=1:n_distances) model_fluxes(:,:,d) = model_fluxes(:,:,d) / distances_kpc(d)**2.
    
  where(model_fluxes == 0.)
    model_fluxes = -huge(0.)
  elsewhere
    model_fluxes = log10(model_fluxes)
  end where
          
  end subroutine setup_models
  
end module SED_models



 
