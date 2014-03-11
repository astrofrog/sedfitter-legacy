module plotting_routines
  
  use base_constants, only : c_si, microns2m
  use base_pgplot
  use base_image, only : add_line, image
  use base_messages

  use extinction, only : load_av_law,av_law
  use base_fits_sed, only : sed_read,sed_read_interpolate_single,sed_read_interpolate_multiple
  use base_source, only : source,filter

  use parameters_plotting
  
  implicit none
  save
    
  private
  public :: show_sed
  public :: show_convolved_fluxes
  public :: plot_source_data
  public :: setup_window
  public :: plot_fit_info
  
contains
  
  subroutine setup_window(s,f)
    implicit none
    type(source),intent(in) :: s
    type(filter),intent(in) :: f(:)
    real(sp) :: xmin,xmax,ymin,ymax
    
    call check_plotting_parameters_set("setup_window")
 
     select case(trim(wav_mode))
     case('A')
        xmin = log10(minval(f(:)%wavelength_microns,s%valid(:).ne.0)) - wav_margin_min
        xmax = log10(maxval(f(:)%wavelength_microns,s%valid(:).ne.0)) + wav_margin_max
      case('M')
        xmin = log10(wav_min)
        xmax = log10(wav_max)
    end select

     select case(trim(flux_mode))
      case('A')
        ymin = minval(s%log_flux(:)-26.+log10(c_si/(f(:)%wavelength_microns*microns2m)),s%valid(:).ne.0) - flux_margin_min
        ymax = maxval(s%log_flux(:)-26.+log10(c_si/(f(:)%wavelength_microns*microns2m)),s%valid(:).ne.0) + flux_margin_max
      case('M')
        ymin = log10(flux_min)
        ymax = log10(flux_max)
    end select
    
    call pgswin(xmin,xmax,ymin,ymax)

  end subroutine setup_window

  subroutine show_sed(sed_filename,data_wav,data_ap,av,sc,best_fit,im)

    implicit none

    character(len=*),intent(in) :: sed_filename
    real(sp),intent(in) :: data_wav(:),data_ap(:)
    real(sp),intent(in) :: av,sc

    real(sp),allocatable :: wav(:),flux(:),flux_star(:)

    logical,intent(in) :: best_fit

    integer :: id(size(data_ap))

    integer :: i,j
    
    type(image),optional,intent(inout) :: im

    call pgslw(lw_seds)

    if(plot_photosphere.and.(best_fit.or.plot_mode=='I')) then
       call sed_read(sed_filename,wavelengths=wav,flux_photosphere=flux_star,units_requested="ergs/cm^2/s")
    end if

    select case(trim(aperture_mode))
    case('largest')
       call sed_read_interpolate_single(sed_filename,minval(data_ap),wavelengths=wav,flux=flux,&
            & units_requested="ergs/cm^2/s",column="TOTAL_FLUX")
       call plot_sed(wav,flux,av,sc,flux_star,best_fit,im)
    case('largest+smallest')
       call sed_read_interpolate_single(sed_filename,maxval(data_ap),wavelengths=wav,flux=flux,&
            & units_requested="ergs/cm^2/s",column="TOTAL_FLUX")
       call plot_sed(wav,flux,av,sc,flux_star,best_fit,im)
    case('interp') 
       call sed_read_interpolate_multiple(sed_filename,data_wav,data_ap,wavelengths=wav,flux=flux,&
            & units_requested="ergs/cm^2/s",column="TOTAL_FLUX")
       call plot_sed(wav,flux,av,sc,flux_star,best_fit,im)
    case('all')

       id = 0
       do i=1,size(data_ap)

          do j=1,i-1
             if(abs(data_ap(j)-data_ap(i))/data_ap(j) < 0.01 ) then
                id(i) = id(j)
                exit
             end if
          end do

          if(id(i) == 0) then
             id(i) = maxval(id) + 1
             call sed_read_interpolate_single(sed_filename,data_ap(i),wavelengths=wav,flux=flux,&
                  & units_requested="ergs/cm^2/s",column="TOTAL_FLUX")
             if(best_fit.or.plot_mode=='I') then
               call pgsci(10+id(i))
             else
               call pgsci(30+id(i))
             end if
             call plot_sed(wav,flux,av,sc,flux_star,best_fit,im)
             flux_star = 0.
          end if

       end do
    case default
       call error("show_sed","unknown aperture mode : "//trim(aperture_mode))
    end select
    
  end subroutine show_sed
  
    subroutine plot_sed(wav,flux,av,sc,flux_star,best_fit,im)
 
      implicit none
      real(sp),intent(in) :: wav(:),flux(:),av,sc
      real(sp),intent(in),allocatable :: flux_star(:)
      logical,intent(in) :: best_fit
      type(image),optional,intent(inout) :: im
      integer :: current_ci
      
      call load_av_law(wav)
      
      if(.not.best_fit.and.plot_greyscale) then
        if(.not.present(im)) call error("plot_sed","no image provided")
         call add_line(im,size(wav),log10(wav),log10(flux(:)+1.e-30)-2.*sc+av*av_law)
      else
         call pgline(size(wav),log10(wav),log10(flux(:)+1.e-30)-2.*sc+av*av_law)
      end if
      
      if(plot_photosphere.and.(best_fit.or.plot_mode=='I')) then
        call pgqci(current_ci)
        call pgsci(1)
        call pgsls(2)
        call pgline(size(wav),log10(wav),log10(flux_star(:)+1.e-30)-2.*sc+av*av_law)
        call pgsls(1)
        call pgsci(current_ci)
      end if

    end subroutine plot_sed

    subroutine plot_fit_info(fit_id,chi,av,sc,model_name)

      implicit none

      integer,intent(in) :: fit_id
      real(sp),intent(in) :: chi,av,sc

      character(len=100) :: label

      character(len=*),intent(in) :: model_name
      
      call pgsch(ch_labels)
      call pgslw(lw_box)
        
      call pgmtxt('T',-3.0,0.5,0.5,"Model : "//trim(model_name))

      if(fit_id==1) then     
         call pgmtxt('T',-4.5,0.5,0.5,"Best fit")
      else
         write(label,'("Fit : ",I0)') fit_id
         call pgmtxt('T',-4.5,0.5,0.5,label)
      end if

      write(label,'("\gx\u2\d = ",F10.3,"    Av = ",F5.1,"    Scale = ",F5.2)') chi,av,sc
      call pgmtxt('T',-6.0,0.5,0.5,label)

    end subroutine plot_fit_info

    subroutine plot_source_data(s,f)

      implicit none

      type(source),intent(in) :: s
      type(filter),intent(in) :: f(:)
      
      integer :: j

      real(sp),allocatable :: plot_wav(:),plot_flux(:),plot_error(:)
      
      allocate(plot_wav(s%n_wav),plot_flux(s%n_wav),plot_error(s%n_wav))
      
      plot_wav   = log10(f%wavelength_microns)
      plot_flux  = s%log_flux-26.+log10(c_si/(f%wavelength_microns*microns2m))
      plot_error = s%log_flux_error
      
      call pgsci(1)

      do j=1,s%n_wav
         select case(s%valid(j))
         case(1,4)
            call pgslw(15)
            call pgpt1(plot_wav(j),plot_flux(j),-1)
            call pgslw(1)
            call pgerr1(6,plot_wav(j),plot_flux(j),plot_error(j),0.)
         case(2)
            call pgsch(1.20) ; call pgslw(3)
            call pgpt1(plot_wav(j),plot_flux(j),852)
            call pgsch(0.75) ; call pgslw(1)
         case(3)
            call pgsch(1.20) ; call pgslw(3)
            call pgpt1(plot_wav(j),plot_flux(j),854)
            call pgsch(0.75) ; call pgslw(1)
         case(9)
            call pgpt1(plot_wav(j),plot_flux(j),4)
         end select
      end do
      
      deallocate(plot_wav,plot_flux,plot_error)

    end subroutine plot_source_data

    subroutine show_convolved_fluxes(wav,log_flux)
      implicit none
      real(sp),intent(in) :: wav(:),log_flux(:)
      real(sp),allocatable :: plot_wav(:),plot_flux(:)

      call pgslw(lw_seds)
      
      allocate(plot_wav(size(wav)),plot_flux(size(wav)))
      
      plot_wav   = log10(wav)
      plot_flux  = log_flux-26.+log10(c_si/(wav*microns2m))

      call pgpt(size(wav),plot_wav,plot_flux,22)
      call pgline(size(wav),plot_wav,plot_flux)

    end subroutine show_convolved_fluxes

end module plotting_routines
