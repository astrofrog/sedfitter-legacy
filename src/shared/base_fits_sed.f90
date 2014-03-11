module base_fits_sed

  use base_cfitsio
  use base_messages
  use base_string
  use base_constants
  use base_array

  implicit none
  save

  private
  
  public :: sed_read
  public :: sed_read_single
  public :: sed_read_interpolate_single
  public :: sed_read_interpolate_multiple
  
  public :: sed_create,sed_close
  public :: sed_write_apertures
  public :: sed_write_wavelengths
  public :: sed_write_stellar_spectrum
  public :: sed_add_fluxes

  integer,parameter :: bitpix = -32
  logical,parameter :: extend = .true.

contains

  subroutine sed_create(unit,filename,model_name)
    implicit none
    integer,intent(out) :: unit
    character(len=*),intent(in) :: filename,model_name
    character(len=100)          :: title(0)="",form(0)="",units(0)=""
    call fits_open_new(unit,filename)
    call fits_write_primary_header(unit,bitpix,1,(/0/),extend)
    call fits_write_keyword(unit,"VERSION",        1, "Format version")
    call fits_write_keyword(unit,"MODEL", model_name, "Model name")
    call fits_write_keyword(unit,"IMAGE",    .false., "Whether images are available in HDU 0")
    call fits_write_keyword(unit,"WAVLGHTS", .false., "Whether wavelengths are available in HDU 1")
    call fits_write_keyword(unit,"APERTURS", .false., "Whether apertures are available in HDU 2")
    call fits_write_keyword(unit,"SEDS",     .false., "Whether SEDs are available in HDU 3")
    call fits_create_hdu(unit,2)
    call fits_table_write_header(unit,0,2,(/"WAVELENGTH","FREQUENCY "/),(/"1E","1E"/),(/"MICRONS","HZ     "/),"WAVELENGTHS")
    call fits_create_hdu(unit,3)
    call fits_table_write_header(unit,0,1,(/"APERTURE"/),(/"1E"/),("AU"),"APERTURES")
    call fits_create_hdu(unit,4)
    call fits_table_write_header(unit,0,0,title,form,units,"SEDS")
  end subroutine sed_create
  
  subroutine sed_close(unit)
    implicit none
    integer,intent(in) :: unit
    call fits_close(unit)
  end subroutine sed_close
      
  subroutine sed_write_wavelengths(unit,wavelengths)
     implicit none
     integer,intent(in) :: unit
     real(sp),intent(in)             :: wavelengths(:)
     logical                     :: exist_wavelengths
     call fits_move_hdu(unit,1)
     call fits_read_keyword(unit,"WAVLGHTS",exist_wavelengths)
     if(exist_wavelengths) call error("write_wavelengths","wavelength array already exists")
     call fits_write_keyword(unit,"WAVLGHTS",.true.,"Whether wavelengths are available in HDU 1")
     call fits_write_keyword(unit,"NWAV",size(wavelengths),"Number of wavelengths")
     call fits_move_hdu(unit,2)
     call fits_table_write_column(unit,1,size(wavelengths),wavelengths)
     call fits_table_write_column(unit,2,size(wavelengths),c_si/(wavelengths*microns2m))
  end subroutine sed_write_wavelengths
  
  subroutine sed_write_stellar_spectrum(unit,flux_stellar,units)
     implicit none
     integer,intent(in) :: unit
     real(sp),intent(in)             :: flux_stellar(:)
     logical                     :: exist_wavelengths
     character(len=*),intent(in) :: units
     integer :: n_wav
     call fits_move_hdu(unit,1)
     call fits_read_keyword(unit,"WAVLGHTS",exist_wavelengths)
     if(.not.exist_wavelengths) call error("write_wavelengths","wavelength array does not exist")
     call fits_read_keyword(unit,"NWAV",n_wav)
     if(size(flux_stellar).ne.n_wav) call error("write_wavelengths","stellar flux array is not the right size")
     call fits_move_hdu(unit,2)
     call fits_table_new_column(unit,3,"STELLAR_FLUX","1E")
     call fits_write_keyword(unit,"TUNIT3",units)
     call fits_table_write_column(unit,3,n_wav,flux_stellar)
  end subroutine sed_write_stellar_spectrum
  
  subroutine sed_write_apertures(unit,apertures)
    implicit none
    integer,intent(in) :: unit
    real(sp),intent(in)             :: apertures(:)
    logical                     :: exist_apertures
    call fits_move_hdu(unit,1)
    call fits_read_keyword(unit,"APERTURS",exist_apertures)
    if(exist_apertures) call error("write_apertures","aperture array already exists")
    call fits_write_keyword(unit,"APERTURS",.true.,"Whether apertures are available in HDU 2")
    call fits_write_keyword(unit,"NAP",size(apertures),"Number of apertures")
    call fits_move_hdu(unit,3)
    call fits_table_write_column(unit,1,size(apertures),apertures)
  end subroutine sed_write_apertures
  
  subroutine sed_add_fluxes(unit,colname,flux,units)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname,units
    real(sp),intent(in)             :: flux(:,:)
    integer                     :: n_wav,n_ap
    logical                     :: exist_wavelengths
    logical                     :: exist_apertures
    integer                     :: colnum
    call fits_move_hdu(unit,1)
    call fits_read_keyword(unit,"WAVLGHTS",exist_wavelengths)
    if(.not.exist_wavelengths) call error("add_sed","wavelength array absent")
    call fits_read_keyword(unit,"NWAV",n_wav)
    call fits_read_keyword(unit,"APERTURS",exist_apertures)
    if(.not.exist_apertures) call error("add_sed","aperture array absent")
    call fits_read_keyword(unit,"NAP",n_ap)
    if(size(flux,1).ne.n_wav) call error("write_sed","first dimension of fluxes should be wavelength")
    if(size(flux,2).ne.n_ap)  call error("write_sed","second dimension of fluxes should be aperture")
    call fits_write_keyword(unit,"SEDS",.true.,"Whether SEDs are available in HDU 3")
    call fits_move_hdu(unit,4)
    colnum = fits_table_number_columns(unit) + 1
    call fits_table_new_column(unit,colnum,colname,concat("",n_wav,"E"))
    call fits_write_keyword(unit,concat("TUNIT",colnum,""),units)
    call fits_table_write_column(unit,colnum,n_wav*n_ap,flux)
  end subroutine sed_add_fluxes

  subroutine sed_read(filename,wavelengths,apertures,column,flux,units_requested,model_name,flux_photosphere,ascending_wav)
    
    implicit none

    character(len=*),intent(in)           :: filename

    character(len=*),intent(in), optional :: column,units_requested
    character(len=*),intent(out),optional :: model_name
    real(sp),allocatable,intent(out),optional :: wavelengths(:),apertures(:),flux(:,:),flux_photosphere(:)

    logical,optional,intent(in) :: ascending_wav

    integer                     :: n_wav,n_ap
    logical                     :: exist_wavelengths
    logical                     :: exist_apertures

    integer :: colnum,k,j
    character(len=100) :: units
    integer :: unit
    
    call fits_open_read(unit,filename)
    
    if(present(model_name)) call fits_read_keyword(unit,"MODEL",model_name)
    
    call fits_read_keyword(unit,"WAVLGHTS",exist_wavelengths)
    if(.not.exist_wavelengths) call error("add_sed","wavelength array absent")
    call fits_read_keyword(unit,"NWAV",n_wav)

    call fits_read_keyword(unit,"APERTURS",exist_apertures)
    if(.not.exist_apertures) call error("add_sed","aperture array absent")
    call fits_read_keyword(unit,"NAP",n_ap)
    
    call fits_move_hdu(unit,2)

    if(present(wavelengths)) then
      allocate(wavelengths(n_wav))
      call fits_table_read_column(unit,1,n_wav,wavelengths)
    end if
    
    if(present(flux_photosphere)) then
      allocate(flux_photosphere(n_wav))
      if(.not.present(wavelengths))     call error("sed_read","photosphere flux requested - wavelengths required")
      if(.not.present(units_requested)) call error("sed_read","photosphere flux requested - units required")
      call fits_table_read_column(unit,3,n_wav,flux_photosphere)
      call fits_read_keyword(unit,"TUNIT3",units)
      if(units_requested==units) then
         ! nothing to do
       else if(units=='mJy'.and.units_requested=='ergs/cm^2/s') then
         flux_photosphere(:) = flux_photosphere(:) / (wavelengths(:)*microns2m/c_si*ergs2mJy)
       else if(units_requested=='mJy'.and.units=='ergs/cm^2/s') then
         flux_photosphere(:) = flux_photosphere(:) * (wavelengths(:)*microns2m/c_si*ergs2mJy)
       else
         call error("sed_read","unknown units : "//trim(units_requested))
       end if
    end if
    
    call fits_move_hdu(unit,3)
    
    if(present(apertures)) then
      allocate(apertures(n_ap))
      call fits_table_read_column(unit,1,n_ap,apertures)
    end if
    
    call fits_move_hdu(unit,4)
    
    if(present(flux)) then

      allocate(flux(n_wav,n_ap))

      if(.not.present(wavelengths))     call error("sed_read","flux requested - wavelengths required")
      if(.not.present(column))          call error("sed_read","flux requested - column required")
      if(.not.present(units_requested)) call error("sed_read","flux requested - units required")

      colnum = fits_table_column_number(unit,column)
      call fits_read_keyword(unit,concat("TUNIT",colnum,""),units)
      call fits_table_read_column(unit,colnum,n_wav*n_ap,flux)
      if(units_requested==units) then
         ! nothing
       else if(units=='mJy'.and.units_requested=='ergs/cm^2/s') then
         forall(k=1:size(flux,2)) flux(:,k) = flux(:,k) / ( wavelengths(:) * microns2m / c_si * ergs2mJy )
       else if(units_requested=='mJy'.and.units=='ergs/cm^2/s') then
         forall(k=1:size(flux,2)) flux(:,k) = flux(:,k) * ( wavelengths(:) * microns2m / c_si * ergs2mJy )
       else
         call error("sed_read","unknown units : "//trim(units_requested))
       end if

       if(present(ascending_wav)) then
          if(ascending_wav .neqv. wavelengths(n_wav) > wavelengths(1)) then
             forall(j=1:n_wav)
                wavelengths(j) = wavelengths(n_wav+1-j)
                flux(j,:) = flux(n_wav+1-j,:)
             end forall
          end if
       end if

    end if
    
    call fits_close(unit)
 
  end subroutine sed_read

  subroutine sed_read_interpolate_single(filename,aperture,wavelengths,column,flux,units_requested,model_name,flux_photosphere)
  
    implicit none
    
    character(len=*),intent(in) :: filename
    real(sp),intent(in) :: aperture
    
    character(len=*),intent(in),optional  :: column,units_requested
    character(len=*),intent(out),optional :: model_name
    real(sp),allocatable,intent(out),optional :: wavelengths(:),flux(:),flux_photosphere(:)
    
    real(sp),allocatable :: flux_orig(:,:),apertures_orig(:)
    integer :: j,n_wav
      
    if(.not.present(flux)) call error("sed_read_interpolate_single","flux array missing")
    
    call sed_read(filename,wavelengths,apertures_orig,column,flux_orig,units_requested,model_name,flux_photosphere)

    n_wav = size(flux_orig,1)
    allocate(flux(n_wav))
    
    do j=1,n_wav
      if(aperture > maxval(apertures_orig)) then
        flux(j) = flux_orig(j,size(apertures_orig))
      else if(aperture < minval(apertures_orig)) then
        call error("sed_read_interpolate_single","aperture too small")
       else
        flux(j) = interpolate_linear(apertures_orig,flux_orig(j,:),aperture)
      end if
    end do
    
  end subroutine sed_read_interpolate_single
  
  subroutine sed_read_single(filename,ap_id,wavelengths,column,flux,units_requested,model_name,flux_photosphere)
  
    implicit none
    
    character(len=*),intent(in) :: filename
    integer,intent(in) :: ap_id
    
    character(len=*),intent(in),optional  :: column,units_requested
    character(len=*),intent(out),optional :: model_name
    real(sp),allocatable,intent(out),optional :: wavelengths(:),flux(:),flux_photosphere(:)
    
    real(sp),allocatable :: flux_orig(:,:),apertures_orig(:)
    integer :: j,n_wav
      
    if(.not.present(flux)) call error("sed_read_single","flux array missing")
    
    call sed_read(filename,wavelengths,apertures_orig,column,flux_orig,units_requested,model_name,flux_photosphere)

    n_wav = size(flux_orig,1)
    allocate(flux(n_wav))
    
    do j=1,n_wav
        flux(j) = flux_orig(j,ap_id)
    end do
    
  end subroutine sed_read_single
  

  subroutine sed_read_interpolate_multiple(filename,interp_wav_orig,interp_ap_orig,wavelengths,column,flux,&
    & units_requested,model_name,flux_photosphere)
  
    implicit none
    
    character(len=*),intent(in) :: filename
    real(sp),intent(in) :: interp_wav_orig(:),interp_ap_orig(:)
    
    character(len=*),intent(in),optional  :: column,units_requested
    character(len=*),intent(out),optional :: model_name
    real(sp),allocatable,intent(out),optional :: wavelengths(:),flux(:),flux_photosphere(:)
    
    real(sp),allocatable :: flux_orig(:,:),apertures_orig(:)
    real(sp) :: aperture_needed
    integer :: j,n_wav
            
    integer :: n_interp
    
    integer,dimension(size(interp_wav_orig)) :: order
    real(sp),   dimension(size(interp_wav_orig)) :: interp_wav,interp_ap
  
    call index_array_1d(size(interp_wav_orig),interp_wav_orig,order)
    
    interp_wav = interp_wav_orig(order)
    interp_ap  = interp_ap_orig(order)
    
    if(.not.present(flux)) call error("sed_read_interpolate_single","flux array missing")
    if(.not.present(wavelengths)) call error("sed_read_interpolate_single","wavelengths array missing")
    
    n_interp = size(interp_wav)
            
    call sed_read(filename,wavelengths,apertures_orig,column,flux_orig,units_requested,model_name,flux_photosphere)

    n_wav = size(flux_orig,1)
    allocate(flux(n_wav))
    
    do j=1,n_wav
      if(wavelengths(j) < interp_wav(1)) then
        aperture_needed = interp_ap(1)
      else if(wavelengths(j) > interp_wav(n_interp)) then
        aperture_needed = interp_ap(n_interp)
      else
        aperture_needed = interpolate_log(interp_wav,interp_ap,wavelengths(j))
      end if
      
      if(aperture_needed > maxval(apertures_orig)) then
        flux(j) = flux_orig(j,size(apertures_orig))
      else if(aperture_needed < minval(apertures_orig)) then
        call error("sed_read_interpolate_multiple","aperture too small")
      else
        flux(j) = interpolate_linear(apertures_orig,flux_orig(j,:),aperture_needed)
      end if
    end do
    
  end subroutine sed_read_interpolate_multiple

end module base_fits_sed
