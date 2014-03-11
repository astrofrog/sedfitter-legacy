program plot_params_2d

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

  use base_image

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
  character(len=100) :: parameter_name_x,parameter_name_y
  real(sp),allocatable :: values_all_x(:),values_x(:)
  real(sp),allocatable :: values_all_y(:),values_y(:)
  ! Parameter file, name, and values

  real(sp) :: xmin,xmax,ymax,ymin,xrange,yrange
  ! Range of values  

  logical :: log_scale_x,show_zero_x
  logical :: log_scale_y,show_zero_y
  ! Whether to plot on a log scale, and whether to show zero values

  logical :: auto_scale_x = .false.
  logical :: auto_scale_y = .false.

  character(len=100) :: xlabel = "X label"
  character(len=100) :: ylabel = "Y label"
  ! Axis labels

  integer :: unit_in,n_wav

  type(image) :: grey_main
  type(image) :: grey_zero_x
  type(image) :: grey_zero_y

  logical :: web

  if(.not.present_arg('par_file'))  call display_usage
  if(.not.present_arg('input'))     call display_usage
  if(.not.present_arg('output'))    call display_usage
  if(.not.present_arg('parameterx')) call display_usage
  if(.not.present_arg('parametery')) call display_usage
  if(.not.present_arg('logx'))       call display_usage
  if(.not.present_arg('logy'))       call display_usage
  if(logical_arg('logx').and..not.present_arg('zerox')) call display_usage
  if(logical_arg('logy').and..not.present_arg('zeroy')) call display_usage

  if(present_arg('web')) then
     web = logical_arg('web')
  else
     web = .false.
  end if

  call read_plotting_parameters(char_arg('par_file'),'param_2d')

  ! Get command-line options

  input_file       = char_arg('input')
  output_directory = char_arg('output')
  parameter_name_x = char_arg('parameterx')
  parameter_name_y = char_arg('parametery')

  if(present_arg('xlabel')) then
     xlabel = char_arg("xlabel")
  else
     call get_label(parameter_name_x,xlabel)
  end if

  if(present_arg('ylabel')) then
     ylabel = char_arg("ylabel")
  else
     call get_label(parameter_name_y,ylabel)
  end if

  log_scale_x = logical_arg('logx')
  log_scale_y = logical_arg('logy')

  if(log_scale_x) then
     show_zero_x = logical_arg('zerox')
  else
     show_zero_x = .false.
  end if

  if(log_scale_y) then
     show_zero_y = logical_arg('zeroy')
  else
     show_zero_y = .false.
  end if

  call delete_dir(output_directory)

  call open_output_file_read(input_file,unit_in,n_wav,models_dir=models_directory)
  call set_source_size(s,n_wav)

  ! Set parameter file name

  parameter_file = trim(models_directory)//'/parameters.fits'

  ! Get all parameter values

  ! Get all parameter values

  select case(trim(parameter_name_x))
  case('AV','LOGD','AVTOT')
     if(allocated(values_all_x)) deallocate(values_all_x)
     allocate(values_all_x(0))
  case default
     call get_parameter_values_all(parameter_file,parameter_name_x,values_all_x)
  end select

  select case(trim(parameter_name_y))
  case('AV','LOGD','AVTOT')
     if(allocated(values_all_y)) deallocate(values_all_y)
     allocate(values_all_y(0))
  case default
     call get_parameter_values_all(parameter_file,parameter_name_y,values_all_y)
  end select

  where(values_all_x < 0.) values_all_x = -100.
  where(values_all_y < 0.) values_all_y = -100.

  if(log_scale_x) then
     where(values_all_x > 0.)
        values_all_x = log10(values_all_x)
     elsewhere
        values_all_x = -100.
     end where
  end if

  if(log_scale_y) then
     where(values_all_y > 0.)
        values_all_y = log10(values_all_y)
     elsewhere
        values_all_y = -100.
     end where
  end if

  if(size(values_all_x) > 0.) then
     xmin = minval(values_all_x,values_all_x > -100.)
     xmax = maxval(values_all_x,values_all_x < +huge(1.))
  else
     auto_scale_x = .true.
  end if

  if(size(values_all_y) > 0.) then
     ymin = minval(values_all_y,values_all_y > -100.)
     ymax = maxval(values_all_y,values_all_y < +huge(1.))
  else
     auto_scale_y = .true.
  end if

  call message_section("Plotting...")

  do source_id=1,number_sources(unit_in)

     ! Read source + fits

     call read_output_file(unit_in,source_id,s,model_id,model_names,av,sc,chi,out_form,out_number)

     ! Get parameter values for fits

     select case(trim(parameter_name_x))
     case('AV')
        if(allocated(values_x)) deallocate(values_x)
        allocate(values_x(size(av))) ; values_x = av
     case('LOGD')
        if(allocated(values_x)) deallocate(values_x)
        allocate(values_x(size(sc))) ; values_x = sc
     case('AVTOT')
        call get_parameter_values(parameter_file,'AVINT',model_id,model_names,values_x)
        values_x = values_x + av
     case default
        call get_parameter_values(parameter_file,parameter_name_x,model_id,model_names,values_x)
     end select

     select case(trim(parameter_name_y))
     case('AV')
        if(allocated(values_y)) deallocate(values_y)
        allocate(values_y(size(av))) ; values_y = av
     case('LOGD')
        if(allocated(values_y)) deallocate(values_y)
        allocate(values_y(size(sc))) ; values_y = sc
     case('AVTOT')
        call get_parameter_values(parameter_file,'AVINT',model_id,model_names,values_y)
        values_y = values_y + av
     case default
        call get_parameter_values(parameter_file,parameter_name_y,model_id,model_names,values_y)
     end select

     if(log_scale_x) then
        where(values_x > 0.)
           values_x = log10(values_x)
        elsewhere
           values_x = -huge(1._sp)
        end where
     end if

     if(log_scale_y) then
        where(values_y > 0.)
           values_y = log10(values_y)
        elsewhere
           values_y = -huge(1._sp)
        end where
     end if

     if(auto_scale_x) then
        xmin = minval(values_x, mask=values_x > -huge(1._sp))
        xmax = maxval(values_x, mask=values_x > -huge(1._sp))
        xrange = max(xmax-xmin,0.1)
        xmin = xmin - xrange/10.
        xmax = xmax + xrange/10.
        if(trim(parameter_name_x)=='AV' .and. xmin < 0. .and. .not. log_scale_x) xmin = 0.
     end if

     if(auto_scale_y) then
        ymin = minval(values_y, mask=values_y > -huge(1._sp))
        ymax = maxval(values_y, mask=values_y > -huge(1._sp))
        yrange = max(ymax-ymin,0.1)
        ymin = ymin - yrange/10.
        ymax = ymax + yrange/10.
        if(trim(parameter_name_y)=='AV' .and. ymin < 0. .and. .not. log_scale_y) ymin = 0.
     end if

     ! Make plot

     call open_plot(output_directory,s%name,web)
     call pgswin(xmin,xmax,ymin,ymax)
     call pgvsiz(1.5,4.5,1.5,4.5)

     if(source_id==1) then
        call make_greyscale(grey_main,values_all_x,values_all_y,grey_nmaj_2d,grey_nmaj_2d)
     end if

     call plot_image(grey_main,grey_max_2d,0.)

     call pgsch(ch_2dpoint)
     call pgpt(size(values_x),values_x,values_y,17)

     if(show_zero_x.and.show_zero_y) then
        call plot_box('bcstl','bcstl','','')
     else if(show_zero_x) then
        if(log_scale_y) then
           call plot_box('bcnstl','bcstl',xlabel,'')
        else
           call plot_box('bcnstl','bcst',xlabel,'')
        end if
     else if(show_zero_y) then
        if(log_scale_x) then
           call plot_box('bcstl','bcnstvl','',ylabel)
        else
           call plot_box('bcst','bcnstvl','',ylabel)
        end if
     else
        if(log_scale_x.and.log_scale_y) then
           call plot_box('bcnstl','bcnstvl',xlabel,ylabel)
        else if(log_scale_x) then
           call plot_box('bcnstl','bcnstv',xlabel,ylabel)
        else if(log_scale_y) then
           call plot_box('bcnst','bcnstvl',xlabel,ylabel)
        else
           call plot_box('bcnst','bcnstv',xlabel,ylabel)
        end if

     end if

     if(show_source_info) call plot_source_info(s)

     if(show_zero_x) then

        call pgvsiz(1.2,1.4,1.5,4.5)
        call pgswin(-100.1,-99.9,ymin,ymax)

        if(source_id==1) then
           call make_greyscale(grey_zero_x,values_all_x,values_all_y,grey_nmin_2d,grey_nmaj_2d)
        end if

        call plot_image(grey_zero_x,grey_max_2d,0.)

        call pgsch(ch_2dpoint)
        call pgpt(size(values_x),values_x,values_y,17)

        if(log_scale_y) then
           call plot_box('bc','bcnstvl','',ylabel)
        else
           call plot_box('bc','bcnstv','',ylabel)
        end if

        call pgslw(lw_box)
        call pgsch(ch_axis)
        if(log_scale_y.and.show_zero_y) then
           call pgmtxt('B',2.0,0.5,0.5,"0")
        else
           call pgmtxt('B',1.2,0.5,0.5,"0")
        end if
        call pgslw(1)
        call pgsch(1.0)

     end if

     if(show_zero_y) then

        call pgvsiz(1.5,4.5,1.2,1.4)
        call pgswin(xmin,xmax,-100.1,-99.9)

        if(source_id==1) then
           call make_greyscale(grey_zero_y,values_all_x,values_all_y,grey_nmaj_2d,grey_nmin_2d)
        end if

        call plot_image(grey_zero_y,grey_max_2d,0.)

        call pgsch(ch_2dpoint)
        call pgpt(size(values_x),values_x,values_y,17)

        if(log_scale_x) then
           call plot_box('bcnstl','bc',xlabel,'')
        else
           call plot_box('bcnst','bc',xlabel,'')
        end if

        call pgsch(ch_axis)        
        if(log_scale_x.and.show_zero_x) then
           ! already plotted zero
        else
           call pgmtxt('LV',0.7,0.5,1.0,"0")
        end if

     end if

     call pgclos

  end do

  call close_output_file(unit_in)

contains

  subroutine make_greyscale(im,x,y,nx,ny)
    implicit none
    type(image),intent(out) :: im
    real(sp),intent(in) :: x(:),y(:)
    integer,intent(in) :: nx,ny
    integer :: n,i
    type(image) :: im_temp
    print *,'-> setting up image'
    call initialize_image(im_temp,nx,ny)
    n = min(size(x),size(y))
    do i=1,n
       call add_disk(im_temp,x(i),y(i),0.015)
    end do
    call clip_image(im_temp,grey_clip_2d)
    call resample_image(im_temp,im,grey_resample_2d)
  end subroutine make_greyscale

  subroutine display_usage

    implicit none

    write(*,'(" Usage: plot_param_2d [arguments]")')
    write(*,*)
    write(*,'(" Required arguments :")')
    write(*,'("   par_file=filename  - the parameter file (e.g. plot.par)")')
    write(*,'("   input=filename     - the input fitter FITS file")')
    write(*,'("   output=directory   - the output plots directory")')
    write(*,'("   parameterx=name    - the name of the parameter to show on the x axis (e.g. MDISK)")') 
    write(*,'("   parametery=name    - the name of the parameter to show on the y axis (e.g. MDOT)")') 
    write(*,'("   logx=yes/no         - whether to plot the x-axis parameter on a log scale")')
    write(*,'("   logy=yes/no         - whether to plot the y-axis parameter on a log scale")')
    write(*,'("   zerox=yes/no        - if logx==yes, whether to show zero values for the x-axis")')
    write(*,'("   zeroy=yes/no        - if logy==yes, whether to show zero values for the y-axis")')
    write(*,*)
    stop

  end subroutine display_usage

end program plot_params_2d
