module plotting_common

  use base_pgplot
  use base_string, only : clean_string
  use base_messages

  use base_source

  use parameters_plotting

  implicit none
  save

  private
  public :: open_plot,plot_box
  public :: set_colors
  public :: plot_source_info
  public :: get_label

contains

  subroutine open_plot(output_directory,source_name,web,fit_id)

    implicit none

    character(len=*),intent(in) :: output_directory,source_name
    logical,intent(in) :: web
    integer,intent(in),optional :: fit_id

    character(len=path_length) :: prefix,plotname
    character(len=5) :: fit_id_c

    if(web) then
      prefix = "Web_source"
    else
      prefix = source_name
    end if
    
    if(present(fit_id)) then
       write(fit_id_c,'(I5.5)') fit_id
       plotname = trim(output_directory)//"/"//trim(prefix)//'_'//fit_id_c//trim(plot_device)
    else
       plotname = trim(output_directory)//"/"//trim(prefix)//trim(plot_device)
    end if

    call clean_string(plotname)
    if(pgopen(plotname).lt.1) stop
    call set_colors()

  end subroutine open_plot

  subroutine set_colors()

    implicit none
    
    call pgscr(30,shade,shade,shade)

    call pgscr(11,0.65,0.00,0.00)
    call pgscr(12,0.20,0.30,0.80)
    call pgscr(13,1.00,0.30,0.30)
    call pgscr(14,1.00,0.30,0.60)
    call pgscr(15,0.30,0.80,0.30)
    call pgscr(16,0.50,0.10,0.80)
    call pgscr(17,0.20,0.60,0.80)
    call pgscr(18,1.00,0.00,0.00)
    call pgscr(19,0.50,0.25,0.00)
    call pgscr(20,0.90,0.90,0.00)
    call pgscr(21,0.00,0.50,0.00)

    call pgscr(31,1.00,0.70,0.70)
    call pgscr(32,0.70,0.70,0.80)
    call pgscr(33,1.00,0.80,0.70)
    call pgscr(34,1.00,0.75,1.00)
    call pgscr(35,0.70,0.80,0.70)
    call pgscr(36,0.75,0.60,0.80)
    call pgscr(37,0.70,0.75,0.80)
    call pgscr(38,1.00,0.70,0.80)
    call pgscr(39,0.90,0.80,0.70)
    call pgscr(40,0.90,0.90,0.70)
    call pgscr(41,0.50,0.90,0.50)

  end subroutine set_colors

  subroutine plot_source_info(s)
    implicit none
    type(source),intent(in) :: s
    call pgsch(ch_labels)
    call pgslw(lw_box)
    call pgmtxt('T',-1.5,0.5,0.5,s%name)
    call pgsch(1.0)
    call pgslw(1)
  end subroutine plot_source_info

  subroutine plot_box(xform,yform,xlabel,ylabel)

    use base_string
    implicit none

    character(len=*),intent(in) :: xform,yform,xlabel,ylabel
    
    call pgsci(1)
    call pgsls(1)
    call pgslw(lw_box)

    call pgsch(ch_axis)
    call pgbox_i(xform,0.,0,yform,0.,0)

    call pgsch(ch_labels)
    call pgmtxt('B',label_dx,0.5,0.5,xlabel)
    call pgmtxt('L',label_dy,0.5,0.5,ylabel)

    call pgslw(1)
    call pgsch(1.0)

  end subroutine plot_box

  subroutine get_label(short,long)

    implicit none

    character(len=*),intent(in) :: short
    ! short parameter name

    character(len=*),intent(out) :: long
    ! long parameter name

    long=''

    select case(trim(adjustl(short)))

    case('TIME') 
       long='Stellar age (yr)'
    case('MASSC')
       long='Stellar mass (M\d\(2281)\u)'
    case('RSTAR')
       long='Stellar radius (R\d\(2281)\u)'
    case('TSTAR','T_EFF') 
       long='Stellar temperature (K)'
    case('MDOT') 
       long='Envelope accretion rate (M\d\(2281)\u/yr)'
    case('RMAX') 
       long='Envelope outer radius (AU)'
    case('RMINE') 
       long='Envelope inner radius (R\dsub\u)'
    case('THETA')
       long='Envelope cavity angle (degrees)'
    case('MDISK')
       long='Disk mass (M\d\(2281)\u)'
    case('RMAXD')
       long='Disk outer radius (AU)'
    case('RMIND')
       long='Disk inner radius (R\dsub\u)'
    case('RMIND(AU)','RMINDAU') 
       long='Disk inner radius (AU)'
    case('RC') 
       long='Centrifugal radius (AU)'
    case('RCHOLE') 
       long='RCHOLE'
    case('ZMIN') 
       long='Scaleheight factor'
    case('A') 
       long='\ga exponent'
    case('B')
       long='\gb exponent'
    case('ALPHA') 
       long='Disk \ga parameter'
    case('RHOCONST') 
       long='Cavity density (cgs)'
    case('RHOAMB') 
       long='Ambient density (cgs)'
    case('MDOTDISK') 
       long='Disk accretion rate (M\d\(2281)\u/yr)'
    case('INCL.','INCL') 
       long='Inclination (degrees)'
    case('AV_INT')
       long='Total extinction to stellar surface'
    case('LTOT') 
       long='Total luminosity (L\d\(2281)\u)'
    case('H100') 
       long='Disk scaleheight at 100AU (AU)'
    case('AVINT')
       long='A_v [circumstellar] (mag)'
    case('AVTOT')
       long='A_v [interstellar+circumstellar] (mag)'
    case('AV')
       long='A_v [interstellar] (mag)'
    case('LOGD')
       long='Log[d/1kpc]'
    case('MENV')
       long='Envelope and Ambient mass (M\d\(2281)\u)'
    case('LOG[G]')
       long='Log[g]'
    case('[Z/H]')
       long='Log[Z/H]'
    case default
       call error("get_label","no long name available for "//trim(short))
    end select

  end subroutine get_label


end module plotting_common
