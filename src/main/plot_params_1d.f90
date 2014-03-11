program plot_params_1d

  use base_io, only : char_arg, logical_arg, present_arg, delete_dir
  use base_array
  use base_pgplot
  use base_messages

  use base_fitter_output
  use base_source
  use parameters_models, only : read_model_parameters,get_filename_sed

  use parameters_plotting
  use plotting_common
  use base_fits_parameters

  implicit none

  character(len=path_length) :: input_file,output_directory
  character(len=path_length) :: models_directory
  ! Input/Output filenames

  integer :: source_id
  type(source) :: s
  real(sp),allocatable :: av(:),sc(:),chi(:)
  integer,allocatable :: model_id(:)
  character(len=30),allocatable :: model_names(:)
  ! Source and fits info

  character(len=path_length) :: parameter_file
  character(len=100) :: parameter_name
  real(sp),allocatable :: values_all(:),values(:)
  ! Parameter file, name, and values

  real(sp) :: xmin,xmax,xrange,norm
  real(sp),allocatable :: hist_x(:),hist_y(:),hist_y_all(:)  
  real(sp),allocatable :: hist_x_zero(:),hist_y_zero(:),hist_y_all_zero(:)
  ! Histograms

  logical :: log_scale,show_zero
  ! Whether to plot on a log scale, and whether to show zero values

  character(len=100) :: xlabel = ""
  character(len=100) :: ylabel = "Normalized number of models"
  ! Axis labels

  integer :: unit_in,n_wav

  logical :: web
  
  logical :: stats
  real    :: mean,variance
  character(len=100) :: stats_label

  logical :: dynamic_range = .false.

  if(.not.present_arg('par_file'))  call display_usage
  if(.not.present_arg('input'))     call display_usage
  if(.not.present_arg('output'))    call display_usage
  if(.not.present_arg('parameter')) call display_usage
  if(.not.present_arg('log'))       call display_usage
  if(logical_arg('log').and..not.present_arg('zero')) call display_usage

  if(present_arg('web')) then
    web = logical_arg('web')
  else
    web = .false.
  end if

  call read_plotting_parameters(char_arg('par_file'),'param_1d')

  allocate(hist_x(hist_n_bin),hist_x_zero(hist_n_bin/10))
  allocate(hist_y(hist_n_bin),hist_y_zero(hist_n_bin/10))
  allocate(hist_y_all(hist_n_bin),hist_y_all_zero(hist_n_bin/10))

  hist_y = 0. ;  hist_y_all = 0. ; hist_y_zero = 0. ;  hist_y_all_zero = 0.

  ! Get command-line options

  input_file       = char_arg('input')
  output_directory = char_arg('output')
  parameter_name   = char_arg('parameter')

  call delete_dir(output_directory)

  log_scale        = logical_arg('log')
  if(log_scale) show_zero = logical_arg('zero')

  if(present_arg('xlabel')) then
     xlabel = char_arg("xlabel")
  else
     call get_label(parameter_name,xlabel)
  end if
  
  if(present_arg('stats')) then
    if(log_scale.and.show_zero) call error("plot_params_1d","can't calculate stats with log/zero combination")
    stats = logical_arg('stats')
  else
    stats = .false.
  end if

  call open_output_file_read(input_file,unit_in,n_wav,models_dir=models_directory)
  call set_source_size(s,n_wav)

  ! Set parameter file name

  parameter_file = trim(models_directory)//'/parameters.fits'

  ! Get all parameter values

  select case(trim(parameter_name))
  case('AV','LOGD','AVTOT')
    allocate(values_all(0))
  case default
    call get_parameter_values_all(parameter_file,parameter_name,values_all)
  end select

  ! Make histogram

  if(size(values_all) > 0) then

    if(log_scale) then

       if(show_zero) call list2hist(values_all,-1.e-30,+1.e-30,hist_n_bin/10,hist_x_zero,hist_y_all_zero)

       where(values_all > 0.)
          values_all = log10(values_all)
       elsewhere
          values_all = -huge(1.)
       end where

    end if

    xmin = minval(values_all,values_all > -1e30)
    xmax = maxval(values_all,values_all < +1e30)
    xrange = xmax-xmin

    xmin = xmin - xrange/10.
    xmax = xmax + xrange/10.

    call list2hist(values_all,xmin,xmax,hist_n_bin,hist_x,hist_y_all)

    norm = max(maxval(hist_y_all),maxval(hist_y_all_zero))
  
    hist_y_all      = hist_y_all      / norm
    hist_y_all_zero = hist_y_all_zero / norm

  else

    xmin = 0.
    xmax = 0.
    dynamic_range = .true.
        
  end if

  call message_section("Plotting...")

  do source_id=1,number_sources(unit_in)

     ! Read source + fits

     call read_output_file(unit_in,source_id,s,model_id,model_names,av,sc,chi,out_form,out_number)

     ! Get parameter values for fits

     select case(trim(parameter_name))
     case('AV')
       allocate(values(size(av))) ; values = av
     case('LOGD')
       allocate(values(size(sc))) ; values = sc
     case('AVTOT')
       call get_parameter_values(parameter_file,'AVINT',model_id,model_names,values)
       values = values + av
     case default
       call get_parameter_values(parameter_file,parameter_name,model_id,model_names,values)
     end select
     
     ! Make histogram

     if(log_scale) then
        if(show_zero) call list2hist(values,-1.e-30,+1.e-30,hist_n_bin/10,hist_x_zero,hist_y_zero)
        where(values > 0.)
           values = log10(values)
        elsewhere
           values = -huge(1.)
        end where
     end if
     
     if(dynamic_range) then
       xmin = minval(values)
       xmax = maxval(values)
       if(xmin==xmax) then
         xmin = values(1)*0.9
         xmax = values(1)*1.1
       else
         xrange = xmax-xmin
         xmin = xmin - xrange/10.
         xmax = xmax + xrange/10.
       end if
       if(trim(parameter_name)=='AV' .and. xmin < 0.) xmin = 0.
     end if

     call list2hist(values,xmin,xmax,hist_n_bin,hist_x,hist_y)

     ! Make plot

     call open_plot(output_directory,s%name,web)

     call pgswin(xmin,xmax,0.,1.2)

     call pgvsiz(vxmin,vxmax,vymin,vymax)
     
     norm = max(maxval(hist_y),maxval(hist_y_zero))

     if(norm==0.) then
       call pgslw(2)
       call pgsch(0.75)
       call pgmtxt('T',-1.5,0.975,1.00,"No models")
     else
       hist_y      = hist_y      / norm
       hist_y_zero = hist_y_zero / norm
     end if

     if(size(values_all)>0) call plot_hist(hist_n_bin,hist_x,hist_y_all,1,30)
     call plot_hist(hist_n_bin,hist_x,hist_y,3,1)
     call pgbin(hist_n_bin,hist_x,hist_y,.true.)

     if(show_source_info) call plot_source_info(s)
     
     ! Compute stats if needed
     
     if(stats) then
       if(size(values)>1) then
         mean     = sum(values,values>-100.) / count(values>-100.)
         variance = sum((values-mean)**2.,values>-100.) / count(values>-100.)
         write(stats_label,'(F5.2,"+/-",F5.2)') mean,sqrt(variance)
         call pgslw(2)
         call pgsch(0.75)
         call pgmtxt('T',-1.5,0.025,0.00,trim(stats_label))
         call plot_normal(mean,variance,1000)
       end if
     end if

     if(log_scale.and.show_zero) then

        call plot_box('bcnstl','bcst',xlabel,'')

        call pgvsiz(vxmin-(vxmax-vxmin)/10.-0.1,vxmin-0.1,1.,4.)

        call pgswin(-1.e-30,+1.e-30,0.,1.2)

        call plot_hist(hist_n_bin/10,hist_x_zero,hist_y_all_zero,1,30)
        call plot_hist(hist_n_bin/10,hist_x_zero,hist_y_zero,3,1)
        call pgbin(hist_n_bin/10,hist_x_zero,hist_y_zero,.true.)

        call plot_box('bc','bcstnv','',ylabel)

        call pgslw(lw_box)
        call pgsch(ch_axis)
        call pgmtxt('B',1.2,0.5,0.5,"0")
        call pgslw(1)
        call pgsch(1.0)
        
     else

        if(log_scale) then
           call plot_box('bcnstl','bcnstv',xlabel,ylabel)
        else
           call plot_box('bcnst','bcnstv',xlabel,ylabel)
        end if

     end if

     call pgclos

  end do

  call close_output_file(unit_in)

contains
  
  subroutine plot_normal(mean,variance,n)
    implicit none
    real(sp),intent(in)    :: mean,variance
    integer,intent(in) :: n
    real               :: x(n),y(n)
    real               :: xmin,xmax,ymin,ymax
    integer            :: i,lw
    call pgqwin(xmin,xmax,ymin,ymax)
    do i=1,n
      x(i) = real(i-1)/real(n-1) * (xmax-xmin) + xmin
      y(i) = exp(-(x(i)-mean)**2/2./variance)
    end do
    call pgqlw(lw)
    call pgslw(3)
    call pgline(n,x,y)
    call pgslw(lw)
  end subroutine plot_normal

  subroutine display_usage

    implicit none

    write(*,'(" Usage: plot_param_1d [arguments]")')
    write(*,*)
    write(*,'(" Required arguments :")')
    write(*,'("   par_file=filename  - the parameter file (e.g. plot.par)")')
    write(*,'("   input=filename     - the input fitter FITS file")')
    write(*,'("   output=directory   - the output plots directory")')
    write(*,'("   parameter=name     - the name of the parameter to show (e.g. MDISK)")') 
    write(*,'("   log=yes/no         - whether to plot the parameter on a log scale")')
    write(*,'("   zero=yes/no        - if log==yes, whether to show zero values")')
    write(*,*)
    stop

  end subroutine display_usage

end program plot_params_1d
