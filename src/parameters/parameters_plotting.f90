module parameters_plotting

  use base_types
  use base_messages
  use base_parfile
  use base_source

  implicit none
  save

  private
  public :: read_plotting_parameters
  public :: check_plotting_parameters_set

  ! Common
  
  logical,private :: set = .false.
  ! whether the parameters have been set

  real(sp),parameter,public :: vxmin = 1.5
  real(sp),parameter,public :: vxmax = 5.5
  real(sp),parameter,public :: vymin = 1.
  real(sp),parameter,public :: vymax = 4.

  character(len=1),public :: out_form = ""
  real(sp),public             :: out_number = 0.

  logical,public :: show_source_info = .false.

  real(sp),public    :: ch_labels = 0.
  real(sp),public    :: ch_axis = 0.

  integer,public :: lw_box = 0
  integer,public :: lw_seds = 0

  character(len=10),public :: plot_device

  real(sp),public :: shade

  real(sp),public :: label_dx = 0.
  real(sp),public :: label_dy = 0.

  ! SEDs

  character(len=1),public :: plot_mode = ""

  logical,public :: plot_greyscale = .false.
  integer,public :: grey_nx=0,grey_ny=0
  real(sp),public    :: grey_clip=0.,grey_max=0.

  logical,public :: plot_photosphere = .false.

  character(len=6),public :: wav_mode = ""
  real(sp),public    :: wav_margin_min = 0.
  real(sp),public    :: wav_margin_max = 0.
  real(sp),public    :: wav_min = 0.
  real(sp),public    :: wav_max = 0.
  
  character(len=6),public :: flux_mode = ""
  real(sp),public    :: flux_margin_min = 0.
  real(sp),public    :: flux_margin_max = 0.
  real(sp),public    :: flux_min = 0.
  real(sp),public    :: flux_max = 0.
  
  logical,public :: plot_convolved_fluxes = .false.
  logical,public :: plot_seds = .false.
  
  character(len=20),public :: aperture_mode = ""

  logical,public :: show_fit_info = .false.

  ! Histogram

  integer,public :: hist_n_bin

  ! 2D plot

  integer,public :: grey_resample_2d = 0
  integer,public :: grey_nmaj_2d     = 0
  integer,public :: grey_nmin_2d     = 0
  real(sp),public    :: grey_clip_2d     = 0.
  real(sp),public    :: grey_max_2d      = 0.
  real(sp),public    :: ch_2dpoint       = 0.

contains

  subroutine read_plotting_parameters(filename,origin)

    implicit none

    character(len=*),intent(in) :: filename,origin

    call message_section("Plotting parameters")

    call load_par_file(filename)

    ! Shared parameters (whatever program)

    call read_par('oform',out_form)
    call read_par('onumb',out_number)
    
    if(origin=='generic') return

    call read_par('pname',show_source_info)
    call read_par('chaxi',ch_axis)
    call read_par('chlab',ch_labels)
    call read_par('lwbox',lw_box)
    call read_par('lwsed',lw_seds)
    call read_par('devic',plot_device)
    call read_par('shade',shade)
    call read_par('labdx',label_dx)
    call read_par('labdy',label_dy)

    ! SEDs

    select case(origin)
    case('sed')

       call read_par('pmode',plot_mode)

       select case(plot_mode)
       case('A')
          call read_par('pgrey',plot_greyscale)
          call read_par('greyx',grey_nx)
          call read_par('greyy',grey_ny)
          call read_par('greyc',grey_clip)
          call read_par('greym',grey_max)
       case('I')
       case default
          call error("read_plotting_parameters","unknown plotting mode : "//plot_mode)
       end select

       if(out_form=='N'.and.nint(out_number)==1.and.plot_greyscale) then
          call error("read_plotting_parameters","greyscale mode not useful for N 1")
       end if

       call read_par('pconv',plot_convolved_fluxes)
       call read_par('pseds',plot_seds)
    
       if(plot_seds) call read_par('stype',aperture_mode)

       call read_par('xmode',wav_mode)
       select case(trim(wav_mode))
       case('A')
          call read_par('xmina',wav_margin_min)
          call read_par('xmaxa',wav_margin_max)
       case('M')
          call read_par('xminm',wav_min)
          call read_par('xmaxm',wav_max)
       case default
          call error("read_plotting_parameters","unknown mode : "//trim(wav_mode))
       end select
    
       call read_par('ymode',flux_mode)
       select case(trim(flux_mode))
       case('A')
          call read_par('ymina',flux_margin_min)
          call read_par('ymaxa',flux_margin_max)
       case('M')
          call read_par('yminm',flux_min)
          call read_par('ymaxm',flux_max)
       case default
          call error("read_plotting_parameters","unknown mode : "//trim(flux_mode))
       end select
  
       call read_par('patmo',plot_photosphere)
       call read_par('pinfo',show_fit_info)
    
       ! Check for incompatibilities

       if(plot_greyscale .and. plot_convolved_fluxes) then
          call error("read_plotting_parameters","cannot plot convolved fluxes in greyscale mode")
       end if
       
       if(plot_mode=='A'.and.show_fit_info) then
          call warning("read_plotting_parameters","cannot plot fit information for A mode")
       end if
    
    case('param_1d')

       call read_par('histn',hist_n_bin)
       
    case('param_2d')

       call read_par('greyspl2d',grey_resample_2d)
       call read_par('greybig2d',grey_nmaj_2d)
       call read_par('greysma2d',grey_nmin_2d)
       call read_par('greycli2d',grey_clip_2d)
       call read_par('greymax2d',grey_max_2d)
       call read_par('ch2dpoint',ch_2dpoint)

    case default

       call error("read_plotting_parameters","unknown origin : "//trim(origin))
       
    end select

    set = .true.

  end subroutine read_plotting_parameters
  
    subroutine check_plotting_parameters_set(origin)
      implicit none
      character(len=*),intent(in) :: origin
      if(.not.set) call error(origin,"plotting parameters not set")
    end subroutine check_plotting_parameters_set

  end module parameters_plotting

