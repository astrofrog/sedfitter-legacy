!#############################################################################
! Purpose : convolve SEDs in Fnu format (including polarization spectra)
! Author  : Thomas Robitaille
! Date    : 15th July 2006
! Checked : Yes
! Known Issues: None
!#############################################################################

program convolve

  use base_fits_sed
  use base_filters
  use base_io
  use base_messages
  use base_fits_convolved_fluxes
  use base_constants

  implicit none

  real,allocatable :: wav(:),ap(:),flux(:,:),flux_err(:,:)
  type(filter),allocatable :: filters(:)
  character(len=path_length),allocatable :: files(:)
  character(len=100),allocatable :: model_name(:)

  integer :: m,k,f

  character(len=10) :: out_letter

  integer :: n_models
  integer :: n_apertures
  integer :: n_filters

  real,allocatable :: final_apertures(:)
  real,allocatable :: convolved_flux(:,:,:)
  real,allocatable :: convolved_flux_error(:,:,:)

  real           :: ram_usage
  real,parameter :: ram_limit = 1.8 ! Gb

  character(len=1000) :: out_file, models_dir, models_seds, models_convolved

  integer :: unit,j

  models_dir = trim(char_arg('models_dir'))//'/'

  if(present_arg('seds')) then
    models_seds = trim(models_dir)//'/'//trim(char_arg('seds'))//'/'
  else
    models_seds = trim(models_dir)//'/seds/'
  end if

  if(present_arg('seds')) then
    models_convolved = trim(models_dir)//'/'//trim(char_arg('convolved'))//'/'
  else
    models_convolved = trim(models_dir)//'/convolved/'
  end if
    
  if(present_arg('append')) then
    if(.not.logical_arg('append')) then
      call delete_dir(models_convolved)
    end if
  else
    call delete_dir(models_convolved)
  end if

  if(logical_arg('compressed')) then
    call list_files(models_seds,files,"*.fits.gz")
  else
    call list_files(models_seds,files,"*.fits")
  end if

  if(size(files)==0) call error("convolve","no files to convolve")

  n_models = size(files)
  allocate(model_name(n_models))

  call read_filters(char_arg('filters'),filters)
  n_filters = size(filters)

  do m=1,size(files)

    print *,'Convolving ',trim(files(m))

    call sed_read(files(m),wavelengths=wav,apertures=ap,&
         &column="TOTAL_FLUX",flux=flux,units_requested="mJy",&
         &model_name=model_name(m),ascending_wav=.false.)

    call sed_read(files(m),wavelengths=wav,apertures=ap,&
         &column="TOTAL_FLUX_ERR",flux=flux_err,units_requested="mJy",&
         &ascending_wav=.false.)

    if(m==1) then
      allocate(final_apertures(size(ap)))
      final_apertures = ap
      n_apertures = size(final_apertures)
      ram_usage = n_models*n_apertures*n_filters*4.e-9 * 2
      if(ram_usage > ram_limit) call error("convolve","not enough RAM")
      allocate(convolved_flux(n_apertures,n_models,n_filters))
      allocate(convolved_flux_error(n_apertures,n_models,n_filters))
    else
      if(.not.all(final_apertures==ap)) then
        call error("convolve","all SEDs must be defined in the same apertures")
      end if
    end if

    call rebin_filters(filters,real(c_si/(wav*microns2m)))

    forall(f=1:size(filters),k=1:size(ap))
      convolved_flux(k,m,f)       = sum(flux(:,k)*filters(f)%t_va(:))
      convolved_flux_error(k,m,f) = sqrt(sum((flux_err(:,k)*filters(f)%t_va(:))**2.))
    end forall

  end do
  
  do f=1,size(filters)
     out_file = trim(models_convolved)//trim(filters(f)%name)//'.fits'
     call write_convolved_fluxes(out_file,final_apertures,model_name(:),convolved_flux(:,:,f),&
     & convolved_flux_error(:,:,f),"mJy",filters(f)%wavelength_microns)
  end do

end program convolve
