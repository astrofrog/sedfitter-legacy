! This example demonstrates how to retrieve all parameters for all fits
! and output an ASCII file. This example will only work with any sets of 
! models provided MODEL_NAMES is the first column in the FITS parameter
! file and that all other parameters are numerical.

program generic_ex2

  use base_types

  use base_io

  use base_fitter_output
  use base_source

  use base_fits_parameters

  use parameters_plotting

  implicit none

  character(len=100) :: input_file,output_file
  character(len=100) :: models_directory
  ! Input/Output filenames

  integer :: source_id
  type(source) :: s
  type(filter),allocatable :: filt(:)
  real(sp),allocatable :: av(:),sc(:),chi(:)
  integer,allocatable :: model_id(:)
  character(len=30),allocatable :: model_name(:)
  ! Source and fits info

  character(len=100) :: parameter_file
  ! parameter file

  type parameter
     character(len=10) :: name = ""
     real(sp),allocatable  :: value(:)
  end type parameter

  type(parameter),allocatable :: par(:)
  ! Array to contain all parameters

  integer :: f,p
  ! loop variables

  integer :: unit,unit_full
  ! I/O unit

  integer :: n_fits,n_par
  ! number of fits and number of parameters

  character(len=10) :: c_n_par
  ! number of parameters in character form

  integer :: unit_in,n_wav
  
  integer :: public(27)
  character(len=7) :: modname
  integer :: inc
  
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

  ! Find number of parameters ignoring first column (assume it's MODEL_NAME)
  n_par = number_parameters(parameter_file) - 1 ; allocate(par(n_par))

  if(n_par.ne.27) then
     stop 'ERROR - n_par != 27'
  else
     public=(/1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,0,1,1,1/)
  end if

  do p=1,n_par
     call get_parameter_name(parameter_file,p+1,par(p)%name)
  end do

  write(c_n_par,'(I10)') n_par

  ! Open output file

  call open_safe(unit,file=output_file,status='new')
  call open_safe(unit_full,file=trim(output_file)//'_full',status='new')

  ! Loop over sources

  do source_id=1,number_sources(unit_in)

     ! Read all fit info for this source

     call read_output_file(unit_in,source_id,s,model_id,model_name,av,sc,chi,out_form,out_number)

     n_fits = size(chi)   

     do p=1,n_par
        call get_parameter_values(parameter_file,par(p)%name,model_id,model_name,par(p)%value)
     end do

     write(unit,'("#############################################################################")')
     write(unit,'(A30)') s%name      ! write out source name
     write(unit,'(I6)')  n_fits      ! number of fits to output for this source

     ! --- OUTPUT MODEL NAME, FIT PARAMETERS, AND ALL MODEL PARAMETERS --- !

      do f=1,n_fits
        read(model_name(f)(1:7),*) modname
        read(model_name(f)(9:10),*) inc
        write(unit,'(I5," ; ",A7," ; ",I2," ; ",F12.2," ; ",2(F7.2," ; "))') f,modname,inc,chi(f),av(f),sc(f)
      end do
      
      
      write(unit_full,'("#############################################################################")')
      write(unit_full,'(A30)') s%name      ! write out source name
      write(unit_full,'(I6)')  n_fits      ! number of fits to output for this source

      write(unit_full,'("FIT # --------- MODEL NAME --------- --- CHI2 --- -- Av -- - Logd -")',advance='no')
      
      do p=1,n_par
         if(public(p)==1) then
            write(unit_full,'(1X,"- ",A10," -")',advance='no') par(p)%name
         end if
      end do
      
      write(unit_full,*)

       do f=1,n_fits
          read(model_name(f)(1:7),*) modname
          write(unit_full,'(I5,1X,A30,1X,F12.2,1X,2(F8.2,1X))',advance='no')  f,modname,chi(f),av(f),sc(f)

         do p=1,n_par
            if(public(p)==1) then
               write(unit_full,'(1X,2X,ES10.3,2X)',advance='no') par(p)%value(f)
            end if
         end do

         write(unit_full,*)

       end do

  end do

  close(unit=unit)
  close(unit=unit_full)

  call close_output_file(unit_in)


contains

  subroutine display_usage

    implicit none

    write(*,'(" Usage: generic_ex2 [arguments]")')
    write(*,*)
    write(*,'(" Required arguments :")')
    write(*,'("   par_file=filename  - the parameter file (e.g. plot.par)")')
    write(*,'("   input=filename     - the input fitter FITS file")')
    write(*,'("   output=filename    - the output filename")')
    write(*,*)
    stop

  end subroutine display_usage

end program generic_ex2
