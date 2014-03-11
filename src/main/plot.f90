program plotsed

  use base_io, only : char_arg, delete_dir,present_arg,logical_arg
  use base_image, only : initialize_image,clip_image,plot_image,image
  use base_pgplot
  use base_messages

  use base_fitter_output
  use base_source
  use parameters_models, only : read_model_parameters,get_filename_sed

  use performance

  use extinction, only : set_av_file

  use parameters_plotting
  use plotting_common
  use plotting_routines

  implicit none

  character(len=1000) :: input_file,sed_filename,output_directory
  ! filenames

  type(source) :: s
  type(filter),allocatable :: f(:)
  ! source and filters

  real(sp),allocatable :: av(:),sc(:),chi(:),model_fluxes(:,:)
  integer,allocatable :: model_id(:)
  character(len=30),allocatable :: model_names(:)
  ! model fitting results

  integer :: source_id,fit_id
  ! loop variables

  character(len=1000) :: models_directory,file_ex_law

  real(sp),allocatable :: apertures_au(:)

  type(image) :: grey_seds

  integer :: unit_in,n_wav
  
  logical :: web

  if(.not.present_arg('par_file')) call display_usage
  if(.not.present_arg('input'))    call display_usage
  if(.not.present_arg('output'))   call display_usage

  if(present_arg('web')) then
    web = logical_arg('web')
  else
    web = .false.
  end if

  call read_plotting_parameters(char_arg('par_file'),'sed')

  input_file       = char_arg('input')
  output_directory = char_arg('output')

  call delete_dir(output_directory)

  call open_output_file_read(input_file,unit_in,n_wav,models_dir=models_directory,ex_law=file_ex_law,filters=f)
  call set_source_size(s,n_wav)

  call read_model_parameters(models_directory)
  allocate(apertures_au(size(f)))

  call set_av_file(file_ex_law)

  call message_section("Plotting...")
  call cpu_init

  do source_id=1,number_sources(unit_in)

     if(plot_convolved_fluxes) then
        call read_output_file(unit_in,source_id,s,model_id,model_names,av,sc,chi,out_form,&
             &out_number,model_fluxes=model_fluxes)
     else
        call read_output_file(unit_in,source_id,s,model_id,model_names,av,sc,chi,out_form,&
             &out_number) 
     end if

     if(plot_mode=='A') then
        call open_plot(output_directory,s%name,web)
        call pgvsiz(vxmin,vxmax,vymin,vymax)
        call setup_window(s,f)
        if(plot_greyscale) call initialize_image(grey_seds,grey_nx,grey_ny)
        call pgsci(30)
     end if

     do fit_id=size(chi),1,-1

        if(plot_mode=='I') then
           call open_plot(output_directory,s%name,web,fit_id)
           call pgvsiz(vxmin,vxmax,vymin,vymax)
           call setup_window(s,f)
        end if

        if(plot_mode=='A'.and.fit_id.eq.1) then
           call pgsci(1)
           if(plot_greyscale) then
              call clip_image(grey_seds,grey_clip)
              call plot_image(grey_seds,grey_max,0.)
           end if
        end if

        apertures_au = f%aperture_arcsec*1000.*10.**(sc(fit_id))

        if(plot_seds) then
           call get_filename_sed(models_directory,model_names(fit_id),sed_filename)
           call show_sed(sed_filename,f%wavelength_microns,apertures_au,&
                &             av(fit_id),sc(fit_id),best_fit=fit_id==1,im=grey_seds)
        end if

        if(plot_convolved_fluxes) then
           call show_convolved_fluxes(f%wavelength_microns,model_fluxes(:,fit_id))
        end if

        if(plot_mode=='I') then
           call pgsci(1)
           if(show_fit_info) then
              call plot_fit_info(fit_id,chi(fit_id),av(fit_id),sc(fit_id),model_names(fit_id))
           end if
           if(show_source_info) call plot_source_info(s)     
           call plot_source_data(s,f)     
           call plot_box("bcnstl","bcnstlv","\gl (\gmm)","\glF\d\gl\u (ergs/cm\u2\d/s)")
           call pgclos

        end if

     end do

     if(plot_mode=='A') then
        call pgsci(1)
        if(show_source_info) call plot_source_info(s)     
        call plot_source_data(s,f)     
        call plot_box("bcnstl","bcnstlv","\gl (\gmm)","\glF\d\gl\u (ergs/cm\u2\d/s)")
        call pgclos

     end if

     call cpu_display(source_id)

  end do

  call cpu_display_last(source_id-1)

  call close_output_file(unit_in)

  ! later on, need to implement checks to make sure for example that wavelengths are in correct order etc for interpolation,
  ! that there is sufficient spacing between wavelengths/apertures so change isn't too sudden.

contains

  subroutine display_usage

    implicit none

    write(*,'(" Usage: plot [arguments]")')
    write(*,*)
    write(*,'(" Required arguments :")')
    write(*,'("   par_file=filename  - the parameter file (e.g. plot.par)")')
    write(*,'("   input=filename     - the input fitter FITS file")')
    write(*,'("   output=directory   - the output plots directory")')
    write(*,*)
    stop

  end subroutine display_usage

end program plotsed





