! This example demonstrates how to retrieve a few specific parameters
! and output an ASCII file. This example will only work with the R06
! YSO models

program generic_ex1

  use base_types
  
  use base_io

  use base_fitter_output
  use base_source

  use base_fits_parameters

  use parameters_plotting

  implicit none

  character(len=path_length) :: input_file,output_file
  character(len=path_length) :: models_directory
  ! Input/Output filenames

  integer :: source_id
  type(source) :: s
  type(filter),allocatable :: filt(:)
  real(sp),allocatable :: av(:),sc(:),chi(:)
  integer,allocatable :: model_id(:)
  character(len=30),allocatable :: model_name(:)
  ! Source and fits info

  character(len=path_length) :: parameter_file
  ! parameter file

  real(sp),allocatable :: mstar(:),mdisk(:),mdot(:)
  ! example parameter values

  integer :: f
  ! loop variables

  integer :: unit
  ! I/O unit

  integer :: n_fits
  ! number of fits

  integer :: unit_in,n_wav

  ! Check that command-line options are available

  if(.not.present_arg('par_file'))  call display_usage
  if(.not.present_arg('input'))     call display_usage
  if(.not.present_arg('output'))    call display_usage

  call read_plotting_parameters(char_arg('par_file'),'generic')

  ! Get command-line options

  input_file   = char_arg('input')
  output_file  = char_arg('output')

  ! Delete output file if existent

  call delete_file(output_file)

  ! Open input file

  call open_output_file_read(input_file,unit_in,n_wav,models_dir=models_directory,filters=filt)
  call set_source_size(s,n_wav)

  parameter_file = trim(models_directory)//'/parameters.fits'

  ! Open output file

  call open_safe(unit,file=output_file,status='new')

  ! Loop over sources

  do source_id=1,number_sources(unit_in)

     ! Read all fit info for this source

     call read_output_file(unit_in,source_id,s,model_id,model_name,av,sc,chi,out_form,out_number)

     n_fits = size(chi)   

     ! Get parameter values

     call get_parameter_values(parameter_file,"MASSC",model_id,model_name,mstar)
     call get_parameter_values(parameter_file,"MDISK",model_id,model_name,mdisk)
     call get_parameter_values(parameter_file,"MDOT", model_id,model_name, mdot)

     write(unit,'("################################################")')
     write(unit,'(A30)') s%name      ! write out source name
     write(unit,'(I6)')  n_fits      ! number of fits to output for this source

     ! --- OUTPUT MODEL NAME, FIT PARAMETERS, MSTAR, MDISK, AND MDOT --- !

     write(unit,'("FIT # --------- MODEL NAME --------- --- CHI2 --- -- Av -- &
          &- Logd - --- MSTAR --- --- MDISK --- --- MDOT ----")')

     do f=1,n_fits
        write(unit,'(I5,1X,A30,1X,F12.2,1X,2(F8.2,1X),3(2X,ES10.3,2X))') &
             & f,model_name(f),chi(f),av(f),sc(f),mstar(f),mdisk(f),mdot(f)
     end do

  end do

  close(unit=unit)

  call close_output_file(unit_in)

contains

  subroutine display_usage

    implicit none

    write(*,'(" Usage: generic_ex1 [arguments]")')
    write(*,*)
    write(*,'(" Required arguments :")')
    write(*,'("   par_file=filename  - the parameter file (e.g. plot.par)")')
    write(*,'("   input=filename     - the input fitter FITS file")')
    write(*,'("   output=filename    - the output filename")')
    write(*,*)
    stop

  end subroutine display_usage

end program generic_ex1
