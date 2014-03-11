program sn_filter

  use base_types
  use base_io
  use base_messages
  use base_string
  use base_source

  implicit none

  character(len=1000) :: in_data,out_data
  ! input/output files

  type(source) :: s
  ! source variable

  integer :: source_id,n_sources
  ! number of sources and loop variable

  real(sp),allocatable,dimension(:) :: min_sn
  ! fractional errors (actual, and minimum)

  integer :: unit_in,unit_out
  ! file units

  if(.not.present_arg('n_wav'))    call display_usage
  if(.not.present_arg('par_file')) call display_usage
  if(.not.present_arg('input'))    call display_usage
  if(.not.present_arg('output'))   call display_usage

  in_data        = char_arg('input')
  out_data       = char_arg('output')

  call set_source_size(s,integer_arg('n_wav'))

  call read_column(char_arg('par_file'),1,min_sn)

  if(size(min_sn).ne.s%n_wav) then
     call error("sn_filter","incorrect number of lines in parameter file")
  end if

  n_sources = file_n_lines(in_data)

  print '("---------------------------------------------------------")'
  print '(" Input file  : ",A30)',in_data
  print '(" Output file : ",A30)',out_data
  print '(" Signal-to-noise requirements :")'
  print concat('(1X,',s%n_wav,'(F4.1,1X))'),min_sn
  print '("---------------------------------------------------------")'

  if(file_exists(out_data)) then
     call delete_file(out_data)
     print '("---------------------------------------------------------")'
  end if

  call open_safe(unit_in, file=in_data, status='old')
  call open_safe(unit_out,file=out_data,status='new')

  do source_id=1,n_sources

     call read_source_ascii(unit_in,s)

     where(s%flux/s%flux_error < min_sn .and. s%valid==1)
        s%valid = 0
        s%flux  = 0.
        s%flux_error = 0.
     end where

     if(any(s%valid > 0)) call write_source_ascii(unit_out,s)

  end do

  close(unit_in)
  close(unit_out)

contains

  subroutine display_usage()

    implicit none

    write(*,'(" Usage: sn_filter [arguments]")')
    write(*,*)
    write(*,'(" Required arguments :")')
    write(*,'("   n_wav=value         - number of wavelengths")')
    write(*,'("   par_file=filename  - the parameter file (e.g. config/sn_requirements.par)")')
    write(*,'("   input=filename     - the input fitter data file")')
    write(*,'("   output=filename    - the output fitter data file")')
    write(*,*)
    stop

  end subroutine display_usage

end program sn_filter
