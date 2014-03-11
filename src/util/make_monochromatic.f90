!#############################################################################
! Purpose : convolve SEDs in Fnu format (including polarization spectra)
! Author  : Thomas Robitaille
! Date    : 15th July 2006
! Checked : Yes
! Known Issues: None
!#############################################################################

program make_monochromatic

  use base_io
  use base_messages
  use base_constants

  use base_fits_sed
  use base_fits_convolved_fluxes

  implicit none

  real,allocatable :: wav(:),ap(:),flux(:,:),flux_err(:,:)
  character(len=path_length),allocatable :: files(:)
  character(len=100),allocatable :: model_name(:)

  integer :: m,k,f

  character(len=10) :: out_letter

  integer :: n_models
  integer :: n_apertures
  integer :: n_filters
  integer :: n_wav

  real,allocatable :: final_apertures(:)
  real,allocatable :: convolved_flux(:,:,:)
  real,allocatable :: convolved_flux_error(:,:,:)

  real           :: ram_usage
  real,parameter :: ram_limit = 1.8 ! Gb

  character(len=1000) :: out_file, models_dir, models_seds, models_convolved

  character(len=5) :: name
  
  integer :: unit,j,j1,j2,u_list

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

  call sed_read(files(1),wavelengths=wav,apertures=ap)

  n_apertures = size(ap)
  n_wav       = size(wav)

  allocate(final_apertures(n_apertures))
  final_apertures = ap

  n_filters = min(n_wav,floor(ram_limit*1.e9 / (4. * 2. * n_models * n_apertures)))
  
  if(n_filters==0) call error("convolve","not enough RAM")

  allocate(convolved_flux(n_apertures,n_models,n_filters))
  allocate(convolved_flux_error(n_apertures,n_models,n_filters))

  call open_safe(u_list,trim(models_dir)//'/monochromatic_filters.txt',status='new')

  do j1=1,n_wav,n_filters

    j2 = min(j1+n_filters-1,n_wav)

    print *,'Wavelengths ',j1,' to ',j2

    do m=1,size(files)

      call sed_read(files(m),wavelengths=wav,apertures=ap,&
           &column="TOTAL_FLUX",flux=flux,units_requested="mJy",&
           &model_name=model_name(m),ascending_wav=.false.)

      call sed_read(files(m),wavelengths=wav,apertures=ap,&
           &column="TOTAL_FLUX_ERR",flux=flux_err,units_requested="mJy",&
           &ascending_wav=.false.)

      if(.not.all(final_apertures==ap)) then
        call error("convolve","all SEDs must be defined in the same apertures")
      end if

      forall(k=1:size(ap))
         convolved_flux(k,m,:)       = flux(j1:j2,k)
         convolved_flux_error(k,m,:) = flux_err(j1:j2,k)
      end forall

    end do
  
    do f=1,n_filters
       write(name,'("MO",I3.3)') j1+f-1
       write(u_list,'(A5," ",ES12.4)') name,wav(j1+f-1)
       out_file = trim(models_convolved)//trim(name)//'.fits'
       call write_convolved_fluxes(out_file,final_apertures,model_name(:),convolved_flux(:,:,f),&
       & convolved_flux_error(:,:,f),"mJy",wav(j1+f-1))
    end do

    deallocate(final_apertures)
    deallocate(convolved_flux)
    deallocate(convolved_flux_error)

  end do

  close(u_list)

end program make_monochromatic
