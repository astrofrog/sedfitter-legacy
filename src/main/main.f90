program fitter

  use base_io
  use base_messages

  use parameters_fitting
  use parameters_models
  use parameters_data
  use parameters_output

  use base_source

  use performance

  use base_fitter_output, only : open_output_file_new,close_output_file
  use extinction, only         : set_av_file,load_av_law
  use fitting_procedures, only : fit_data
  use SED_models, only         : setup_models,distance_required

  implicit none

  type(source) :: s

  integer :: unit,i
  integer :: n_sources
  integer :: out_unit
  
  logical :: noform

  character(len=1000) :: par_file

  if(.not.present_arg('par_file')) call display_usage

  par_file = char_arg('par_file')

  call read_fitting_parameters(par_file,distance_required)
  call read_output_parameters(par_file)
  call read_data_parameters(par_file)

  call read_model_parameters(models_directory,distance_required)
  call setup_models()

  call check_fitting_parameters_set("main")
  call check_model_parameters_set("main")
  call check_data_parameters_set("main")
  call check_output_parameters_set("main")

  call print_filters(filt)

  call set_av_file(filename_ex_law)

  call load_av_law(filt%wavelength_microns)

  call open_output_file_new(out_file,out_unit,models_directory,filename_ex_law,filt,output_model_fluxes,&
       &                       out_format,out_number)

  call set_source_size(s,n_wav)

  if(present_arg('noform')) then
    noform = logical_arg('noform')
  else
    noform = .false.
  end if

  call message_section("Fitting...")          

  n_sources = file_n_lines(data_file)

  call cpu_init

  call open_safe(unit,file=data_file,status='old')
  do i=1,n_sources
     call read_source_ascii(unit,s,noform)
     if(s%n_data >= min_data) call fit_data(s,out_unit)
     call cpu_display(i)
  end do
  close(unit)

  call cpu_display_last(n_sources)
  
  call close_output_file(out_unit)

contains

  subroutine display_usage

    implicit none

    write(*,'(" Usage: fit [arguments]")')
    write(*,*)
    write(*,'(" Required arguments :")')
    write(*,'("   par_file=filename  - the parameter file (e.g. config/sn_requirements.par)")')
    write(*,*)
    write(*,'(" Optional arguments :")')
    write(*,'("   output=filename    - the output directory")')
    write(*,'("   input=filename     - the input fitter data file")')
    write(*,'("   models=directory   - the models to use ")')
    write(*,'("   best=yes/no        - whether to output only the best fit")')
    write(*,*)
    stop

  end subroutine display_usage


end program fitter
