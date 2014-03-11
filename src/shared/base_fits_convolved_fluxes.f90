module base_fits_convolved_fluxes

  use base_types
  use base_cfitsio
  use base_messages

  implicit none
  save

  private
  public :: write_convolved_fluxes
  public :: read_convolved_fluxes
  public :: interpolate_convolved_fluxes

  integer,parameter :: bitpix = -32
  logical,parameter :: extend = .true.
  integer :: unit

contains

  subroutine write_convolved_fluxes(filename,apertures,model_names,fluxes,flux_errors,units,wavelength)

    use base_string
    implicit none

    character(len=*),intent(in) :: filename,model_names(:),units
    real(sp),intent(in) :: apertures(:),fluxes(:,:),flux_errors(:,:),wavelength
    
    character(len=30),dimension(5) :: title,form,tunits
    integer :: n_models,n_apertures,m
    real(sp) :: radius_sigma_50(size(fluxes,2)),radius_cumul_99(size(fluxes,2))

    n_apertures = size(fluxes,1)
    n_models    = size(fluxes,2)

    title = ""
    form  = ""
    tunits = ""

    title(1) = "MODEL_NAME"
    title(2) = "TOTAL_FLUX"
    title(3) = "TOTAL_FLUX_ERR"
    title(4) = "RADIUS_SIGMA_50"
    title(5) = "RADIUS_CUMUL_99"
    form(1) = "30A"
    form(2) = concat("",n_apertures,"E")
    form(3) = form(2)
    form(4:5) = "1E"
    tunits(2:3) = units
    tunits(4:5) = "AU"
        
    if(size(apertures).ne.n_apertures) call error("write_convolved flux","flux and aperture array dimensions do not match")

    call fits_open_new(unit,filename)

    call fits_write_primary_header(unit,bitpix,1,(/0/),extend)
    call fits_write_keyword(unit,"FILTWAV",wavelength)
    call fits_write_keyword(unit,"NMODELS",n_models)
    call fits_write_keyword(unit,"NAP",n_apertures)

    call fits_create_hdu(unit,2)

    call fits_table_write_header(unit,0,5,title,form,tunits,"CONVOLVED FLUXES")
    call fits_table_write_column(unit,1,n_models,model_names)
    call fits_table_write_column(unit,2,n_models*n_apertures,fluxes)
    call fits_table_write_column(unit,3,n_models*n_apertures,flux_errors)
    
    if(n_apertures > 1) then
       do m=1,n_models
          call find_radius_sigma(apertures,fluxes(:,m),0.50,radius_sigma_50(m))
          call find_radius_cumul(apertures,fluxes(:,m),0.99,radius_cumul_99(m))
       end do
    else
       radius_sigma_50 = apertures(1)
       radius_cumul_99 = apertures(1)
    end if
    
    call fits_table_write_column(unit,4,n_models,radius_sigma_50)
    call fits_table_write_column(unit,5,n_models,radius_cumul_99)
  
    call fits_create_hdu(unit,3)

    call fits_table_write_header(unit,n_apertures,1,(/"APERTURE"/),(/"1E"/),("AU"),"APERTURES")
    call fits_table_write_column(unit,1,n_apertures,apertures)  

    call fits_close(unit)

  end subroutine write_convolved_fluxes

  subroutine read_convolved_fluxes(filename,apertures,model_names,fluxes,flux_errors,radius_sigma_50,radius_cumul_99,wavelength)

    implicit none

    character(len=*),intent(in) :: filename
    
    character(len=*),allocatable,optional,intent(out) :: model_names(:)
    
    real(sp),allocatable,intent(out),optional :: apertures(:),fluxes(:,:),flux_errors(:,:)
    real(sp),allocatable,intent(out),optional :: radius_sigma_50(:),radius_cumul_99(:)
    real(sp),intent(out),optional :: wavelength

    integer :: n_models,n_apertures

    write(*,'(" Reading")',advance='no')
    write(*,*) trim(filename)

    call fits_open_read(unit,filename)

    if(present(wavelength)) call fits_read_keyword(unit,"FILTWAV",wavelength)
    
    call fits_read_keyword(unit,"NMODELS",n_models)
    call fits_read_keyword(unit,"NAP",n_apertures)

    call fits_move_hdu(unit,2)

    if(present(model_names)) then
       allocate(model_names(n_models))
       call fits_table_read_column(unit,1,n_models,model_names)
    end if

    if(present(fluxes)) then
       allocate(fluxes(n_apertures,n_models))
       call fits_table_read_column(unit,2,n_models*n_apertures,fluxes)
    end if

    if(present(flux_errors)) then
       allocate(flux_errors(n_apertures,n_models))
       call fits_table_read_column(unit,3,n_models*n_apertures,flux_errors)
    end if
    
    if(present(radius_sigma_50)) then
       allocate(radius_sigma_50(n_models))
       call fits_table_read_column(unit,4,n_models,radius_sigma_50)
    end if
    
    if(present(radius_cumul_99)) then
       allocate(radius_cumul_99(n_models))
       call fits_table_read_column(unit,5,n_models,radius_cumul_99)
    end if
   
    call fits_move_hdu(unit,3)
    
    if(present(apertures)) then
       allocate(apertures(n_apertures))
       call fits_table_read_column(unit,1,n_apertures,apertures)
    end if

    call fits_close(unit)

  end subroutine read_convolved_fluxes

  subroutine interpolate_convolved_fluxes(filename,apertures,fluxes,resolved,extended,wavelength)

    use base_array

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: filename
    ! the FITS file to read the models from

    real(sp),dimension(:),intent(in),target :: apertures
    ! the apertures required

    ! --- Output --- !

    real(sp),intent(out) :: fluxes(:,:)
    ! the fluxes interpolated for all models and apertures
    
    real(sp),optional :: wavelength
    
    logical(1),intent(out) :: resolved(:,:),extended(:,:)

    real(sp),allocatable :: radius_sigma_50(:)
    real(sp),allocatable :: radius_cumul_99(:)

    ! --- Local variables --- !

    integer :: n_apertures_file
    real(sp),allocatable,dimension(:),target :: apertures_file
    real(sp),allocatable,dimension(:,:),target :: fluxes_file
    ! the number of apertures, and the apertures defined in the file
    ! the fluxes in the file

    integer :: k

    integer :: pos
    real(sp) :: frac

    real(sp),dimension(:),pointer :: f1,f2
    real(sp),pointer :: x1,x2,x_first,x_last,x

    call read_convolved_fluxes(filename,apertures=apertures_file,fluxes=fluxes_file, &
    & radius_sigma_50=radius_sigma_50,radius_cumul_99=radius_cumul_99,wavelength=wavelength)

    n_apertures_file = size(apertures_file)

    do k=1,size(apertures)
      
      resolved(:,k) = radius_cumul_99(:) > apertures(k)
      extended(:,k) = radius_sigma_50(:) > apertures(k)
      
       x       => apertures(k)
       x_first => apertures_file(1)
       x_last  => apertures_file(size(apertures_file))

       ! here use apertures_file(size(apertures_file))
       ! can this be sped up using OPENMP?
       ! this is not in log space obviously. Maybe get models to be stored in log format? then also makes
       ! interpolation more correct. Also maybe can use bitpix=16 for storage once in log?

       if(x > x_first .and. x < x_last) then

          pos = locate(apertures_file,x)

          x1 => apertures_file(pos)
          x2 => apertures_file(pos+1)
          f1 => fluxes_file(pos,:)
          f2 => fluxes_file(pos+1,:)

          frac = ( x - x1 ) / ( x2 - x1 )

          fluxes(:,k) = f1(:) + ( f2(:) - f1(:) ) * frac

       else if(x <= x_first) then

          fluxes(:,k) = fluxes_file(1,:)

       else

          fluxes(:,k) = fluxes_file(n_apertures_file,:)

       end if

    end do

    deallocate(fluxes_file,apertures_file,radius_cumul_99,radius_sigma_50)

  end subroutine interpolate_convolved_fluxes
  
  subroutine find_radius_cumul(ap,f,fraction,radius)
    
    use base_array
    implicit none
    
    real(sp),intent(in) :: ap(:),f(:),fraction
    real(sp),intent(out) :: radius
    real(sp) :: required
    
    if(f(size(ap)) > 0.) then
      required = fraction * f(size(ap))
      if(required < f(1)) then
        radius = ap(1)
      else
        radius = interpolate_linear(f,ap,required)
      end if
    else
      radius = 0.
    end if
  
  end subroutine find_radius_cumul
      
  subroutine find_radius_sigma(ap,f,fraction,radius)

    implicit none

    real(sp),intent(in) :: ap(:),f(:),fraction
    real            :: sigma(size(f)),maximum
    real(sp),intent(out) :: radius
    integer         :: k

    sigma(1) = f(1)/ap(1)**2
    forall(k=2:size(ap))
      sigma(k) = (f(k)-f(k-1))/(ap(k)**2-ap(k-1)**2)
    end forall
    maximum = maxval(sigma)

    if(maximum > 0.) then
      sigma = sigma / maximum
      do k=size(ap),1,-1
        if(sigma(k)>fraction) then
           if(k==size(ap)) then
              radius = ap(k)
           else
              radius = ap(k)+(ap(k+1)-ap(k))*(sigma(k)-fraction)/(sigma(k)-sigma(k+1))
           end if
          return
        end if
      end do
      call error("find_radius_sigma_frac","can't find radius")
    else
      radius = 0.
      return
    end if

  end subroutine find_radius_sigma

end module base_fits_convolved_fluxes
