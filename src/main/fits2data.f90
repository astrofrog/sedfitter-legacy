program fits2data

  use base_io
  use base_source
  use base_fitter_output

  implicit none

  character(len=1000) :: input_file,output_file
  type(source) :: s
  integer :: source_id
  integer :: unit,unit_in,n_wav

  if(.not.present_arg('input'))  call display_usage
  if(.not.present_arg('output')) call display_usage

  input_file  = char_arg('input')
  output_file = char_arg('output')

  call open_output_file_read(input_file,unit_in,n_wav)
  call open_safe(unit,file=output_file,status='new')

  call set_source_size(s,n_wav)

  do source_id=1,number_sources(unit_in)
     call read_output_file_source(unit_in,source_id,s)
     call write_source_ascii(unit,s)
  end do

  close(unit) 
  call close_output_file(unit_in)

contains

  subroutine display_usage

    implicit none

    write(*,'(" Usage: fits2data [arguments]")')
    write(*,*)
    write(*,'(" Required arguments :")')
    write(*,'("   input=filename     - the input fitter FITS file")')
    write(*,'("   output=filename    - the output data file")')
    write(*,*)
    stop

  end subroutine display_usage

end program fits2data
